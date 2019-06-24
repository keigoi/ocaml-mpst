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
  module Local = Local.Make(Mpst.Dyncheck)(Mpst.LinFlag)(M)(EV)
  module Global = Global.Make(Mpst.Dyncheck)(M)(EV)(C)(NoLin)
  module Util = Util.Make(Mpst.Dyncheck)
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

  let server =
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
        M.async server
      end;
    M.run (testbody arr)

  let dummy_array =
    Bigarray.(Array1.create Int16_unsigned C_layout 1)

  let () =
    if Med.medium = `IPCProcess then begin
        Base.fork_child (fun () -> M.run (server_loop sb)) ();
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

  let server =
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
        M.async (L.run' server)
      end;
    M.run (L.run' testbody arr)

  let dummy_array =
    Bigarray.(Array1.create Int16_unsigned C_layout 1)

  let () =
    if Med.medium = `IPCProcess then begin
        Base.fork_child (fun () -> M.run (L.run' server_loop sb)) ();
      end else if M.is_direct then begin
        ignore (Thread.create (fun () -> M.run (L.run' server_loop sb)) ());
      end

end



module BDirect = Make(struct include P.Pure let run x = x let is_direct = true end)(P.Event)(P.Serial)
module BLwt = Make(struct include ML.P.Lwt let run = Lwt_main.run let is_direct = false end)(ML.P.LwtEvent)(ML.P.LwtSerial)
module BLin = MakeMonad(struct include P.Pure let run x = x let is_direct = true end)(P.Event)(P.Serial)(Linocaml)
module BLinLwt = MakeMonad(struct include ML.P.Lwt let run = Lwt_main.run let is_direct = false end)(ML.P.LwtEvent)(ML.P.LwtSerial)(Linocaml_lwt)
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


let () =
  let open Core in
  let open Core_bench in
  let args = array_sizes in
  Command.run
    (Bench.make_command [
         Bench.Test.create_indexed ~name:"dynamic/direct/shmem" ~args @@ (fun i -> Staged.stage (fun () -> BDirectShmem.test i));
         Bench.Test.create_indexed ~name:"dynamic/direct/untyped" ~args @@ (fun i -> Staged.stage (fun () -> BDirectUntyped.test i));
         Bench.Test.create_indexed ~name:"dynamic/direct/ipc" ~args @@ (fun i -> Staged.stage (fun () -> BDirectIPC.test i));
         Bench.Test.create_indexed ~name:"dynamic/lwt/shmem" ~args @@ (fun i -> Staged.stage (fun () -> BLwtShmem.test i));
         Bench.Test.create_indexed ~name:"dynamic/lwt/untyped" ~args @@ (fun i -> Staged.stage (fun () -> BLwtUntyped.test i));
         Bench.Test.create_indexed ~name:"dynamic/lwt/ipc" ~args @@ (fun i -> Staged.stage (fun () -> BLwtIPC.test i));
         Bench.Test.create_indexed ~name:"static/direct/shmem" ~args @@ (fun i -> Staged.stage (fun () -> BDirectShmem.test i));
         Bench.Test.create_indexed ~name:"static/direct/untyped" ~args @@ (fun i -> Staged.stage (fun () -> BDirectUntyped.test i));
         Bench.Test.create_indexed ~name:"static/direct/ipc" ~args @@ (fun i -> Staged.stage (fun () -> BDirectIPC.test i));
         Bench.Test.create_indexed ~name:"static/lwt/shmem" ~args @@ (fun i -> Staged.stage (fun () -> BLwtShmem.test i));
         Bench.Test.create_indexed ~name:"static/lwt/untyped" ~args @@ (fun i -> Staged.stage (fun () -> BLwtUntyped.test i));
         Bench.Test.create_indexed ~name:"static/lwt/ipc" ~args @@ (fun i -> Staged.stage (fun () -> BLwtIPC.test i));
         (* Bench.Test.create_indexed ~name:"async/shmem" ~args @@ (fun i -> Staged.stage (fun () -> BAsyncShmem.test i));
          * Bench.Test.create_indexed ~name:"async/untyped" ~args @@ (fun i -> Staged.stage (fun () -> BAsyncUntyped.test i));
          * Bench.Test.create_indexed ~name:"async/ipc" ~args @@ (fun i -> Staged.stage (fun () -> BAsyncIPC.test i)); *)
    ])
