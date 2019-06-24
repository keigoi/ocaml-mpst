open Mpst.M
open Mpst.M.Base
module ML = Mpst_lwt.M
module MA = Mpst_async.M

module type MONADRUN = sig
  include S.MONAD
  val run : 'a t -> 'a
  val is_direct : bool
end

module type MEDIUM = sig
  val medium : [`Local | `IPCProcess | `Untyped]
end
module Shmem = struct
  let medium = `Local
end
module IPC = struct
  let medium = `IPCProcess
end
module Untyped = struct
  let medium = `Untyped
end
let ping_or_fini =
  {obj_merge=(fun l r -> object method ping=l#ping method fini=r#fini end);
   obj_splitL=(fun lr -> (lr :> <ping : _>));
   obj_splitR=(fun lr -> (lr :> <fini : _>));
  }

(* array size parameters *)
(* let array_sizes = [4194303 * 2] *)
let array_sizes = [1; 100; 1000; 10000; 100000; 1000000]

(* actual array that is passed around thread/processes *)
let big_arrays =
  List.map (fun size ->
      (size, Bigarray.(Array1.create Int16_unsigned C_layout size)))
    array_sizes


module Make
         (M:MONADRUN)
         (EV:S.EVENT with type 'a monad = 'a M.t)
         (C:S.SERIAL with type 'a monad = 'a M.t)
         (Med:MEDIUM)
       :
sig
  val test : int -> unit
end
  = struct

  module Local = Local.Make(Mpst_monad.Nocheck.Nodyncheck)(Mpst_monad.Nocheck.Noflag)(M)(EV)
  module Global = Global.Make (Mpst_monad.Nocheck.Nodyncheck)(M)(EV)(C)(NoLin)
  module Util = Util.Make(Mpst_monad.Nocheck.Nodyncheck)
              
  (* module Local = Local.Make(Mpst.Dyncheck)(Mpst.LinFlag)(M)(EV)
   * module Global = Global.Make(Mpst.Dyncheck)(M)(EV)(C)(NoLin)
   * module Util = Util.Make(Mpst.Dyncheck) *)
              
  open Global
  open Local
  open Util

  let prot =
    fix (fun t ->
        (a --> b) ping @@
        (b --> a) pong @@ t)

  let sa, sb =
    let g = gen_with_kinds [Med.medium;Med.medium;] prot in
    Global.get_ep a g, Global.get_ep b g


  let (let/) = M.bind

  let testbody =
    let stored = ref sa in
    fun arr ->
      let sa = !stored in
      let/ sa = send sa#role_B#ping arr in
      let/ `pong((), sa) = receive sa#role_B in
      stored := sa;
      M.return_unit

  let server_step =
    let stored = ref sb
    in
    fun () ->
      let sb = !stored in
      let/ `ping(_,sb) = receive sb#role_A in
      let/ sb = send sb#role_A#pong () in
      stored := sb;
      M.return_unit

  let server_loop sb =
    let rec loop sb =
      let/ `ping(_,sb) = receive sb#role_A in
      let/ sb = send sb#role_A#pong () in
      loop sb
    in
    loop sb

  let test i =
    let arr = List.assoc i big_arrays in
    if Med.medium <> `IPCProcess && not M.is_direct then begin
        M.async server_step
      end;
    M.run (testbody arr)

  let dummy_array =
    Bigarray.(Array1.create Int16_unsigned C_layout 1)

  let () =
    if Med.medium = `IPCProcess then begin
        Common.fork_child (fun () -> M.run (server_loop sb)) ();
      end else if M.is_direct then begin
        ignore (Thread.create (fun () -> M.run (server_loop sb)) ());
      end

end

module MakeMonad
         (M:MONADRUN)
         (EV:S.EVENT with type 'a monad = 'a M.t)
         (C:S.SERIAL with type 'a monad = 'a M.t)
         (L:Linocaml.S.S with type 'a IO.io = 'a EV.monad)
         (Med:MEDIUM)
       :
sig
  val test : int -> unit
end
  = struct
  module Local = Mpst_monad.Local_monad.Make(M)(EV)(L)
  module Global = Mpst_monad.Global_monad.Make(M)(EV)(C)(L)
  module Util = Util.Make(Mpst_monad.Nocheck.Nodyncheck)
  open Global
  open Local
  open Util

  let prot =
    fix (fun t ->
        (a --> b) ping @@
        (b --> a) pong @@ t)

  let sa, sb =
    let g = raw_gen_with_kinds [Med.medium;Med.medium;] prot in
    Global.raw_get_ep a g, Global.raw_get_ep b g


  let (let/) = M.bind

  let s = Linocaml.Zero

  let testbody =
    let stored = ref sa in
    let open L in
    fun arr ->
      put_linval s !stored >>= fun () ->
      let%lin #s = s <@ send (fun x->x#role_B#ping) arr in
      let%lin `pong({Linocaml.data=()}, #s) = s <@ receive (fun x -> x#role_B) in
      {__m=(fun pre ->
         stored := (Linocaml.lens_get s pre).__lin;
         M.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
       )}

  let server_step =
    let stored = ref sb
    in
    fun () ->
      let sb = !stored in
      let open L in
      put_linval s sb >>= fun () ->
      let%lin `ping(_,#s) = s <@ receive (fun x->x#role_A) in
      let%lin #s = s <@ send (fun x-> x#role_A#pong) () in
      stored := sb;
      {__m=(fun pre ->
         stored := (Linocaml.lens_get s pre).__lin;
         M.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
       )}

  let server_loop sb =
    let open L in
    let rec loop () =
      let%lin `ping(_,#s) = s <@ receive (fun x ->x#role_A) in
      let%lin #s = s <@ send (fun x -> x#role_A#pong) () in
      loop ()
    in
    put_linval s sb >>= fun () ->
    loop ()

  let test i =
    let arr = List.assoc i big_arrays in
    if Med.medium <> `IPCProcess && not M.is_direct then begin
        M.async (L.run' server_step)
      end;
    M.run (L.run' testbody arr)

  let dummy_array =
    Bigarray.(Array1.create Int16_unsigned C_layout 1)

  let () =
    if Med.medium = `IPCProcess then begin
        Common.fork_child (fun () -> M.run (L.run' server_loop sb)) ();
      end else if M.is_direct then begin
        ignore (Thread.create (fun () -> M.run (L.run' server_loop sb)) ());
      end

end


module BDirectEvent = struct
  let ch_arr = Event.new_channel ()
  let ch_unit = Event.new_channel ()
  let _ : Thread.t =
    Thread.create (fun () ->
        let rec loop () =
          let _ = Event.sync (Event.receive ch_arr) in
          Event.sync (Event.send ch_unit ());
          loop ()
        in loop ()) ()
    
  let test =
    fun i ->
    let arr = List.assoc i big_arrays in
    Event.sync (Event.send ch_arr arr);
    Event.sync (Event.receive ch_unit)
end

module BDirectEventUntyped = struct
  let ch = Event.new_channel ()
  let _:Thread.t =
    Thread.create (fun () ->
        let rec loop () =
          let _ = Event.sync (Event.receive ch) in
          Event.sync (Event.send ch (Obj.repr ()));
          loop ()
        in loop ()) ()

  let test =
    fun i ->
    let arr = List.assoc i big_arrays in
    Event.sync (Event.send ch (Obj.repr arr));
    Event.sync (Event.receive ch)
end

module BDirectEventCont = struct
  let init_ch = Event.new_channel ()

  let _:Thread.t =
    Thread.create (fun () ->
      let rec loop ch =
        let arr_, `Cont(ch) = Event.sync (Event.receive ch) in
        let next = Event.new_channel () in
        Event.sync (Event.send ch ((),`Cont(next)));
        loop next
      in loop init_ch) ()

  let test =
    let stored = ref init_ch in
    fun i ->
    let ch = !stored in
    let arr = List.assoc i big_arrays in
    let next = Event.new_channel () in
    Event.sync (Event.send ch (arr, `Cont(next)));
    let ((),`Cont(ch)) = Event.sync (Event.receive next) in
    stored := ch;
    ()
end

module BBareLwt = struct
  let (let/) = Lwt.bind

  let st1, push1 = Lwt_stream.create ()
  let st2, push2 = Lwt_stream.create ()

  let server_step () =
    let/ arr_ = Lwt_stream.next st1 in
    Lwt.return (push2 (Some ()))

  let test =
    fun i ->
    Lwt.async server_step;
    Lwt_main.run begin
        let arr = List.assoc i big_arrays in
        push1 (Some arr);
        Lwt_stream.next st2
      end
end

module BBareLwtCont = struct
  let (let/) = Lwt.bind

  let init_st, init_push = Lwt_stream.create ()

  let server_step =
    let stored = ref (init_st, init_push) in
    fun () ->
    let st, _ = !stored in
    let/ (arr_,(_,push)) = Lwt_stream.next st in
    let next = Lwt_stream.create () in
    stored := next;
    push (Some((),`Cont(next)));
    Lwt.return_unit

  let test =
    let stored = ref (init_st, init_push) in
    fun i ->
    let _, push = !stored in
    Lwt.async server_step;
    Lwt_main.run begin
        let arr = List.assoc i big_arrays in
        let (st,_) as next = Lwt_stream.create () in
        push (Some (arr, next));
        let/ ((),`Cont(next)) = Lwt_stream.next st in
        stored := next;
        Lwt.return_unit
      end
end

module Make_IPC(M:MONADRUN)(C:S.SERIAL with type 'a monad = 'a M.t) = struct
  module Dpipe = Common.Make_dpipe(C)
               
  let ch = Dpipe.new_dpipe ()

  let () =
    Common.fork_child (fun () ->
        
        let ch = Dpipe.flip_dpipe ch in
        let rec loop () =
          M.bind (C.input_value ch.Dpipe.me.inp) @@ fun _ ->
          M.bind (C.output_value ch.Dpipe.me.out ()) @@ fun _ ->
          M.bind (C.flush ch.Dpipe.me.out) @@ fun () -> 
          loop ()
        in M.run (loop ())) ()

  let test i =
    M.run begin
      let arr = List.assoc i big_arrays in
      M.bind (C.output_value ch.Dpipe.me.out arr) @@ fun () -> 
      M.bind (C.flush ch.Dpipe.me.out) @@ fun () -> 
      C.input_value ch.Dpipe.me.inp
    end
end

module Pure = struct
  include P.Pure
  let run x = x
  let is_direct = true
end
module LwtMonad = struct
  include ML.P.Lwt
  let run = Lwt_main.run
  let is_direct = false
end

module BDirect = Make(Pure)(P.Event)(P.Serial)
module BLwt = Make(LwtMonad)(ML.P.LwtEvent)(ML.P.LwtSerial)
module BLin = MakeMonad(Pure)(P.Event)(P.Serial)(Linocaml)
module BLinLwt = MakeMonad(LwtMonad)(ML.P.LwtEvent)(ML.P.LwtSerial)(Linocaml_lwt)
(* module BAsync =
 *   Make
 *     (Dyncheck)(LinFlag)
 *     (struct
 *       include MA.P.AsyncMonad
 *       let run m =
 *         Async.Deferred.value_exn (Async.Scheduler.within' (fun _ -> m))
 *       let is_direct = false
 *     end)(MA.P.AsyncEvent)(MA.P.AsyncSerial) *)

module BDirectShmem = BDirect(Shmem)
module BDirectIPC = BDirect(IPC)
module BDirectUntyped = BDirect(Untyped)
module BLwtShmem = BLwt(Shmem)
module BLwtIPC = BLwt(IPC)
module BLwtUntyped = BLwt(Untyped)
(* module BAsyncShmem = BAsync(Shmem)
 * module BAsyncIPC = BAsync(IPC)
 * module BAsyncUntyped = BAsync(Untyped) *)
module BLinShmem = BLin(Shmem)
module BLinIPC = BLin(IPC)
module BLinUntyped = BLin(Untyped)
module BLinLwtShmem = BLinLwt(Shmem)
module BLinLwtIPC = BLinLwt(IPC)
module BLinLwtUntyped = BLinLwt(Untyped)

module BBareDirectEventIPC = Make_IPC(Pure)(P.Serial)
module BBareLwtIPC = Make_IPC(LwtMonad)(ML.P.LwtSerial)

let () =
  let open Core in
  let open Core_bench in
  let args = array_sizes in
  Command.run
    Bench.Test.(Bench.make_command [
         create_indexed ~name:"bare/direct/typed" ~args @@ (fun i -> Staged.stage (fun () -> BDirectEvent.test i));
         create_indexed ~name:"bare/direct/untyped" ~args @@ (fun i -> Staged.stage (fun () -> BDirectUntyped.test i));
         create_indexed ~name:"bare/direct/cont" ~args @@ (fun i -> Staged.stage (fun () -> BDirectEventCont.test i));
         create_indexed ~name:"bare/direct/ipc" ~args @@ (fun i -> Staged.stage (fun () -> BBareDirectEventIPC.test i));

         create_indexed ~name:"bare/lwt/typed" ~args @@ (fun i -> Staged.stage (fun () -> BBareLwt.test i));
         create_indexed ~name:"bare/lwt/cont" ~args @@ (fun i -> Staged.stage (fun () -> BBareLwtCont.test i));
         create_indexed ~name:"bare/lwt/ipc" ~args @@ (fun i -> Staged.stage (fun () -> BBareLwtIPC.test i));
         
         create_indexed ~name:"dynamic/direct/shmem" ~args @@ (fun i -> Staged.stage (fun () -> BDirectShmem.test i));
         create_indexed ~name:"dynamic/direct/untyped" ~args @@ (fun i -> Staged.stage (fun () -> BDirectUntyped.test i));
         create_indexed ~name:"dynamic/direct/ipc" ~args @@ (fun i -> Staged.stage (fun () -> BDirectIPC.test i));
         
         create_indexed ~name:"dynamic/lwt/shmem" ~args @@ (fun i -> Staged.stage (fun () -> BLwtShmem.test i));
         create_indexed ~name:"dynamic/lwt/untyped" ~args @@ (fun i -> Staged.stage (fun () -> BLwtUntyped.test i));
         create_indexed ~name:"dynamic/lwt/ipc" ~args @@ (fun i -> Staged.stage (fun () -> BLwtIPC.test i));
         
         create_indexed ~name:"static/direct/shmem" ~args @@ (fun i -> Staged.stage (fun () -> BDirectShmem.test i));
         create_indexed ~name:"static/direct/untyped" ~args @@ (fun i -> Staged.stage (fun () -> BDirectUntyped.test i));
         create_indexed ~name:"static/direct/ipc" ~args @@ (fun i -> Staged.stage (fun () -> BDirectIPC.test i));
         create_indexed ~name:"static/lwt/shmem" ~args @@ (fun i -> Staged.stage (fun () -> BLwtShmem.test i));
         create_indexed ~name:"static/lwt/untyped" ~args @@ (fun i -> Staged.stage (fun () -> BLwtUntyped.test i));
         create_indexed ~name:"static/lwt/ipc" ~args @@ (fun i -> Staged.stage (fun () -> BLwtIPC.test i));
         (* create_indexed ~name:"async/shmem" ~args @@ (fun i -> Staged.stage (fun () -> BAsyncShmem.test i));
          * create_indexed ~name:"async/untyped" ~args @@ (fun i -> Staged.stage (fun () -> BAsyncUntyped.test i));
          * create_indexed ~name:"async/ipc" ~args @@ (fun i -> Staged.stage (fun () -> BAsyncIPC.test i)); *)
    ])
