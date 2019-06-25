open Util

open Mpst.M
open Mpst.M.Base
module ML = Mpst_lwt.M

let fork f x =
  Common.fork_child (fun () ->
      print_endline "process forked";
      f ()) ()

let thread f x =
  ignore (Thread.create (fun () ->
              print_endline "thread started";
              (f x:unit)) ())

(* array size parameters *)
let array_sizes = [1; 100; 1000; 10000; 100000; 1000000]

(* actual array that is passed around threads/processes *)
let big_arrays =
  List.map (fun size ->
      (size, Bigarray.(Array1.create Int16_unsigned C_layout size)))
    array_sizes

let iteration_counts =
  [1; 100; 1000; 10000]
let default_payload = snd @@ List.nth big_arrays 1

module MakeDyn
         (M:PERIPHERAL)
         (Med:MEDIUM)
         ()
       :
sig
  val test_msgsize : int -> (unit -> unit) Core.Staged.t
  val test_iteration : int -> (unit -> unit) Core.Staged.t
end
  = struct

  (* module Local = Local.Make(Mpst_monad.Nocheck.Nodyncheck)(Mpst_monad.Nocheck.Noflag)(M)(M.Event)
   * module Global = Global.Make (Mpst_monad.Nocheck.Nodyncheck)(M)(M.Event)(M.Serial)(NoLin)
   * module Util = Util.Make(Mpst_monad.Nocheck.Nodyncheck) *)

  module Local = Local.Make(Mpst.Dyncheck)(Mpst.LinFlag)(M)(M.Event)
  module Global = Global.Make(Mpst.Dyncheck)(M)(M.Event)(M.Serial)(NoLin)
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

  let test_msgsize i =
    let arr = List.assoc i big_arrays in
    Core.Staged.stage
    @@ fun () ->
       if Med.medium <> `IPCProcess && not M.is_direct then begin
           M.async server_step
         end;
       M.run (testbody arr)

  let server_iter =
    let stored = ref sb in
    fun cnt ->
    let sb = !stored in
    let rec loop sb cnt =
      if cnt = 0 then
        M.return sb
      else begin
          let/ `ping(_,sb) = receive sb#role_A in
          let/ sb = send sb#role_A#pong () in
          (loop[@tailcall]) sb (cnt-1)
        end
    in
    let/ sb = loop sb cnt in
    stored := sb;
    M.return_unit


  let test_iter_body =
    let stored = ref sa in
    fun cnt ->
    let sa = !stored in
    let rec loop sa cnt =
      if cnt = 0 then
        M.return sa
      else begin
          let/ sa = send sa#role_B#ping default_payload in
          let/ `pong((),sa) = receive sa#role_B in
          (loop[@tailcall]) sa (cnt-1)
        end
    in
    let/ sa = loop sa cnt in
    stored := sa;
    M.return_unit


  let test_iteration cnt =
    Core.Staged.stage (fun () ->
        if Med.medium <> `IPCProcess && not M.is_direct then begin
            M.async (fun () -> server_iter cnt)
          end;
        M.run (test_iter_body cnt))

  let () =
    if Med.medium = `IPCProcess then begin
        fork (fun () -> M.run (server_loop sb)) ();
      end else if M.is_direct then begin
        ignore (thread (fun () -> M.run (server_loop sb)) ());
      end

end

module MakeStatic
         (M:PERIPHERAL_LIN)
         (Med:MEDIUM)
         ()
       :
sig
  val test_msgsize : int -> (unit -> unit) Core.Staged.t
  val test_iteration : int -> (unit -> unit) Core.Staged.t
end
  = struct
  module Local = Mpst_monad.Local_monad.Make(M)(M.Event)(M.Linocaml)
  module Global = Mpst_monad.Global_monad.Make(M)(M.Event)(M.Serial)(M.Linocaml)
  module Util = Util.Make(Mpst_monad.Nocheck.Nodyncheck)
  (* module Util = Util.Make(Mpst.M.Dyncheck) *)
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
    let open M.Linocaml in
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
    let open M.Linocaml in
    put_linval s sb >>= fun () ->
    let%lin `ping(_,#s) = s <@ receive (fun x->x#role_A) in
    let%lin #s = s <@ send (fun x-> x#role_A#pong) () in
    stored := sb;
    {__m=(fun pre ->
       stored := (Linocaml.lens_get s pre).__lin;
       M.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
    )}

  let server_loop =
    let open M.Linocaml in
    let stored = ref sb in
    fun cnt ->
    let rec loop cnt =
      if cnt=Some 0 then begin
          return ()
        end else begin
          let%lin `ping(_,#s) = s <@ receive (fun x ->x#role_A) in
          let%lin #s = s <@ send (fun x -> x#role_A#pong) () in
          loop (Mpst.M.Common.map_option (fun x->x-1) cnt)
        end
    in
    let sb = !stored in
    put_linval s sb >>= fun () ->
    loop cnt >>= fun () ->
    {__m=(fun pre ->
       stored := (Linocaml.lens_get s pre).Linocaml.__lin;
       M.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
    )}

  let test_msgsize i =
    let arr = List.assoc i big_arrays in
    Core.Staged.stage (fun () ->
        if Med.medium <> `IPCProcess && not M.is_direct then begin
            M.async (M.Linocaml.run' server_step)
          end;
        M.run (M.Linocaml.run' testbody arr))

  let test_iter_body =
    let open M.Linocaml in
    let stored = ref sa in
    fun cnt ->
    let rec loop cnt =
      if cnt = 0 then
        return ()
      else begin
          let%lin #s = s <@ send (fun x->x#role_B#ping) default_payload in
          let%lin `pong({Linocaml.data=()},#s) = s <@ receive (fun x->x#role_B) in
          (loop[@tailcall]) (cnt-1)
        end
    in
    put_linval s (!stored) >>= fun () ->
    loop cnt >>= fun () ->
    {__m=(fun pre ->
       stored := (Linocaml.lens_get s pre).__lin;
       M.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
    )}


  let test_iteration cnt =
    Core.Staged.stage (fun () ->
        if Med.medium <> `IPCProcess && not M.is_direct then begin
            M.async (fun () -> M.Linocaml.run' server_loop (Some cnt))
          end;
        M.run (M.Linocaml.run' test_iter_body cnt)
      )


  let () =
    if Med.medium = `IPCProcess then begin
        fork (fun () -> print_endline"IPC";M.run (M.Linocaml.run' server_loop None)) ();
      end else if M.is_direct then begin
        ignore (thread (fun () ->print_endline"direct"; M.run (M.Linocaml.run' server_loop None)) ());
      end

end

module BEvent = struct
  let ch_arr = Event.new_channel ()
  let ch_unit = Event.new_channel ()
  let _ : Thread.t =
    Thread.create (fun () ->
        let rec loop () =
          let _ = Event.sync (Event.receive ch_arr) in
          Event.sync (Event.send ch_unit ());
          loop ()
        in loop ()) ()

  let test_msgsize =
    fun i ->
    Core.Staged.stage (fun () ->
        let arr = List.assoc i big_arrays in
        Event.sync (Event.send ch_arr arr);
        Event.sync (Event.receive ch_unit))

  let test_iteration cnt =
    Core.Staged.stage (fun () ->
        let rec loop cnt =
          if cnt = 0 then
            ()
          else begin
              Event.sync (Event.send ch_arr default_payload);
              Event.sync (Event.receive ch_unit);
              loop (cnt-1)
            end
        in loop cnt)
end

module BEventUntyped = struct
  let ch = Event.new_channel ()
  let _:Thread.t =
    Thread.create (fun () ->
        let rec loop () =
          let _ = Event.sync (Event.receive ch) in
          Event.sync (Event.send ch (Obj.repr ()));
          loop ()
        in loop ()) ()

  let test_msgsize i =
    Core.Staged.stage (fun () ->
        let arr = List.assoc i big_arrays in
        Event.sync (Event.send ch (Obj.repr arr));
        Event.sync (Event.receive ch))

  let test_iteration cnt =
    Core.Staged.stage (fun () ->
        let rec loop cnt =
          if cnt=0 then
            ()
          else begin
              Event.sync (Event.send ch (Obj.repr default_payload));
              let _:Obj.t = Event.sync (Event.receive ch) in
              loop (cnt-1)
            end
        in loop cnt
      )
end

module BEventCont = struct
  let init_ch = Event.new_channel ()

  let _:Thread.t =
    Thread.create (fun () ->
        let rec loop ch =
          let arr_, `Cont(ch) = Event.sync (Event.receive ch) in
          let next = Event.new_channel () in
          Event.sync (Event.send ch ((),`Cont(next)));
          loop next
        in loop init_ch) ()

  let test_msgsize =
    let stored = ref init_ch in
    fun i ->
    Core.Staged.stage (fun () ->
        let ch = !stored in
        let arr = List.assoc i big_arrays in
        let next = Event.new_channel () in
        Event.sync (Event.send ch (arr, `Cont(next)));
        let ((),`Cont(ch)) = Event.sync (Event.receive next) in
        stored := ch;
        ())

  let stored = ref init_ch
  let test_iteration cnt =
    Core.Staged.stage @@
      fun () ->
      let rec loop ch cnt =
        if cnt=0 then
          ch
        else begin
            let next = Event.new_channel () in
            Event.sync (Event.send ch (default_payload, `Cont(next)));
            let ((),`Cont(ch)) = Event.sync (Event.receive next) in
            loop ch (cnt-1)
          end
      in
      let ch = !stored in
      let ch = loop ch cnt in
      stored := ch;
      ()
end

module BLwtStream = struct
  let (let/) = Lwt.bind

  (* let ch1 = Lwt_stream.create ()
   * let ch2 = Lwt_stream.create ()
   * let receive (st,_) = Lwt_stream.next st
   * let send (_,push) v = push (Some v); Lwt.return_unit *)
  let ch1 = Lwt_mvar.create_empty ()
  let ch2 = Lwt_mvar.create_empty ()
  let receive m = Lwt_mvar.take m
  let send m v = Lwt_mvar.put m v

  let server_step () =
    let/ arr_ = receive ch1 in
    send ch2 ()

  let test_msgsize =
    fun i ->
    Core.Staged.stage (fun () ->
        Lwt.async server_step;
        Lwt_main.run begin
            let arr = List.assoc i big_arrays in
            let/ () = send ch1 arr in
            receive ch2
          end)


  let server_iter cnt =
    let rec loop cnt =
      if cnt = 0 then
        Lwt.return_unit
      else begin
          let/ arr_ = receive ch1 in
          let/ () = send ch2 () in
          loop (cnt-1)
        end
    in
    loop cnt

  let test_iteration =
    fun cnt ->
    Core.Staged.stage (fun () ->
        Lwt.async (fun () -> server_iter cnt);
        Lwt_main.run begin
            let rec loop cnt =
              if cnt=0 then
                Lwt.return_unit
              else
                let/ () = send ch1 default_payload in
                let/ () = receive ch2 in
                loop (cnt-1)
            in
            loop cnt
          end)
end

module type LWT_CHAN = sig
  type 'a t
  val create : unit -> 'a t
  val send : 'a t -> 'a -> unit Lwt.t
  val receive : 'a t -> 'a Lwt.t
end
module LwtBoundedStream : LWT_CHAN = struct
  type 'a t = 'a Lwt_stream.t * 'a Lwt_stream.bounded_push
  let create () = Lwt_stream.create_bounded 1
  let send (_,wr) v = wr#push v
  let receive (st,_) = Lwt_stream.next st
end
module LwtStream : LWT_CHAN = struct
  type 'a t = 'a Lwt_stream.t * ('a option -> unit)
  let create () = Lwt_stream.create ()
  let send (_,wr) v = wr (Some v); Lwt.return_unit
  let receive (st,_) = Lwt_stream.next st
end
module LwtMVar : LWT_CHAN = struct
  type 'a t = 'a Lwt_mvar.t
  let create () = Lwt_mvar.create_empty ()
  let send m v = Lwt_mvar.put m v
  let receive m = Lwt_mvar.take m
end
module LwtWait : LWT_CHAN = struct
  type 'a t = 'a Lwt.t * 'a Lwt.u
  let create () = Lwt.wait ()
  let send (t_,u) v = Lwt.wakeup_later u v; Lwt.return_unit
  let receive (t,u_) = t
end

module BLwtCont(Chan:LWT_CHAN)() = struct
  open Chan
  let (let/) = Lwt.bind

  (* let init = create () *)

  let server_loop init =
    let stored = ref init in
    fun cnt ->
    let rec loop ch cnt =
      if cnt=0 then
        Lwt.return ch
      else
        let/ arr_,ch = receive ch in
        let next = create () in
        let/ () = send ch (`Next((),next)) in
        loop next (cnt-1)
    in
    let/ ch = loop !stored cnt in
    stored := ch;
    Lwt.return_unit

  let iteration_body init =
    let stored = ref init in
    fun cnt ->
    let rec loop ch cnt =
      if cnt=0 then
        Lwt.return ch
      else begin
          let next = create () in
          let/ () = send ch (default_payload,next) in
          let/ `Next((),ch) = receive next in
          loop ch (cnt-1)
        end
    in
    let/ ch = loop !stored cnt in
    stored := ch;
    Lwt.return_unit

  let test_iteration =
    fun cnt ->
    let init = create () in
    let server_loop = server_loop init in
    let iteration_body = iteration_body init in
    Core.Staged.stage (fun () ->
        Lwt.async (fun () -> server_loop cnt);
        Lwt_main.run begin
            iteration_body cnt
          end)


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

  let test_msgsize =
    let stored = ref (init_st, init_push) in
    fun i ->
    Core.Staged.stage @@
      fun () ->
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

module Make_IPC(M:PERIPHERAL)() = struct
  module Dpipe = Common.Make_dpipe(M.Serial)
  module C = M.Serial

  let ch = Dpipe.new_dpipe ()

  let (let/) = M.bind

  let () =
    fork (fun () ->
        let ch = Dpipe.flip_dpipe ch in
        let rec loop () =
          let/ _ = C.input_value ch.Dpipe.me.inp in
          let/ _ = C.output_value ch.Dpipe.me.out () in
          let/ () = C.flush ch.Dpipe.me.out in
          loop ()
        in M.run (loop ())) ()

  let test_msgsize i =
    Core.Staged.stage @@
      fun () ->
      M.run begin
          let arr = List.assoc i big_arrays in
          let/ () = C.output_value ch.Dpipe.me.out arr in
          let/ () = C.flush ch.Dpipe.me.out in
          C.input_value ch.Dpipe.me.inp
      end

  let test_iteration cnt =
    Core.Staged.stage @@
      fun () ->
      M.run begin
          let rec loop cnt =
            if cnt=0 then
              M.return_unit
            else
              let/ () = C.output_value ch.Dpipe.me.out default_payload in
              let/ () = C.flush ch.Dpipe.me.out in
              let/ () = C.input_value ch.Dpipe.me.inp in
              loop (cnt-1)
          in
          loop cnt
        end

end

module BRefImpl
       :
sig
  val test_msgsize : int -> (unit -> unit) Core.Staged.t
  val test_iteration : int -> (unit -> unit) Core.Staged.t
end
  = struct

  open Mpst_ref

  let prot =
    fix (fun t ->
        (a --> b) ping @@
          (b --> a) pong @@ t)

  let sa, sb =
    let g = gen prot in
    Global.get_ep a g, Global.get_ep b g


  let (let/) m f = f m

  let testbody =
    let stored = ref sa in
    fun arr ->
    let sa = !stored in
    let/ sa = send sa#role_B#ping arr in
    let/ `pong((), sa) = receive sa#role_B in
    stored := sa;
    ()

  let server_loop sb =
    let rec loop sb =
      let/ `ping(_,sb) = receive sb#role_A in
      let/ sb = send sb#role_A#pong () in
      loop sb
    in
    loop sb

  let test_msgsize i =
    Core.Staged.stage (fun () ->
        let arr = List.assoc i big_arrays in
        testbody arr
      )

  let test_iteration =
    let stored = ref sa in
    fun cnt ->
    Core.Staged.stage (fun () ->
        let sa = !stored in
        let rec loop sa cnt =
          if cnt=0 then
            sa
          else
            let/ sa = send sa#role_B#ping default_payload in
            let/ `pong((), sa) = receive sa#role_B in
            loop sa (cnt-1)
        in
        let sa = loop sa (cnt) in
        stored := sa
      )

  let () =
    ignore (Thread.create (fun () -> server_loop sb) ());
end


let test_msgsize =
  let open Core in
  let open Core_bench in
  let args = array_sizes in
  Bench.Test.(
    [
     (* sanity check -- running times for these will not increase *)
     create_indexed ~name:"msgsize/lwt/shmem,chvec" ~args BLwtStream.test_msgsize;
     create_indexed ~name:"msgsize/lwt/shmem,cont" ~args (let module M = BLwtCont(LwtMVar)() in M.test_msgsize);

     create_indexed ~name:"msgsize/mpst-dynamic/lwt/shmem,chvec" ~args (let module M = MakeDyn(LwtMonad)(Shmem)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-dynamic/lwt/shmem,untyped" ~args (let module M = MakeDyn(LwtMonad)(Untyped)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-static/lwt/shmem,chvec" ~args (let module M = MakeStatic(LinLwtMonad)(Shmem)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-static/lwt/shmen,untyped" ~args (let module M = MakeStatic(LinLwtMonad)(Untyped)() in M.test_msgsize);

     create_indexed ~name:"msgsize/ev/shmem,chvec" ~args @@ (fun i -> Staged.stage (fun () -> BEvent.test_msgsize i));
     create_indexed ~name:"msgsize/ev/shmem,untyped" ~args @@ (fun i -> Staged.stage (fun () -> BEventUntyped.test_msgsize i));
     create_indexed ~name:"msgsize/ev/shmem,cont" ~args @@ (fun i -> Staged.stage (fun () -> BEventCont.test_msgsize i));

     create_indexed ~name:"msgsize/ref" ~args @@ (fun i -> Staged.stage (fun () -> BRefImpl.test_msgsize i));

     create_indexed ~name:"msgsize/mpst-dynamic/ev/shmem,chvec" ~args (let module M = MakeDyn(Direct)(Shmem)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-dynamic/ev/shmem,untyped" ~args @@ (let module M = MakeDyn(Direct)(Untyped)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-static/ev/shmem,chvec" ~args @@ (let module M = MakeStatic(LinDirect)(Shmem)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-static/ev/shmem,untyped" ~args @@ (let module M = MakeStatic(LinDirect)(Shmem)() in M.test_msgsize);

     (* running times will increase proportional to the array size *)
     create_indexed ~name:"msgsize/lwt/ipc" ~args (let module M = Make_IPC(LwtMonad)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-dynamic/lwt/ipc" ~args (let module M = MakeDyn(LwtMonad)(IPC)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-static/lwt/ipc" ~args (let module M = MakeStatic(LinLwtMonad)(IPC)() in M.test_msgsize);

     create_indexed ~name:"msgsize/ev/ipc" ~args (let module M = Make_IPC(Direct)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-dynamic/ev/ipc" ~args (let module M = MakeDyn(Direct)(IPC)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-static/ev/ipc" ~args (let module M = MakeStatic(LinDirect)(IPC)() in M.test_msgsize);
  ])

let test_iteration =
  let open Core in
  let open Core_bench in
  let args = iteration_counts in
  Bench.Test.(
      [
        (* running time is propotional to iteration count. *)

        (* Here we compare continuation-passing style communication vs. channel-vector based one.
         * Differences are almost negligible (CPS is around 1 % slower), but apparently CPS allocates
         * more words than two-channel communication.
         *)
        create_indexed ~name:"iter/ev/cont" ~args BEventCont.test_iteration;
        create_indexed ~name:"iter/ev/twochan" ~args BEvent.test_iteration;

        (* Comparions between the bare Event module with ocaml-mpst. this will exhibits overheads in the library.
         * Happily, they are almost negligible when/if:
         * 1) Dynamic linearity checking uses quicker mutex implementation (e.g. Core.Nano_mutex), or
         * 2) Dynamic checkings are removed (and use static checking from Linocaml instead)
         * Linocaml allocates more memory for closures, it does not affect running times.
         *)
        create_indexed ~name:"iter/mpst-ref/ev/shmem" ~args BRefImpl.test_iteration;
        create_indexed ~name:"iter/mpst-dynamic/ev/shmem" ~args (let module M = MakeDyn(Direct)(Shmem)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-static/ev/shmem" ~args @@ (let module M = MakeStatic(LinDirect)(Shmem)() in M.test_iteration);

        (* Lwt is far more faster than Event.
         * Again, we compare CPS with two-channel communication.
         * (It seems that Lwt_mvar is the fastest in CPS-style communication - as for Lwt version 4.2.1.)
         * Still, CPS version is slower than two-channel ver (around 5 %)
         *)
        create_indexed ~name:"iter/lwt(mvar)/cont" ~args (let module M = BLwtCont(LwtMVar)() in M.test_iteration);
        (* create_indexed ~name:"iter/lwt/stream,cont" ~args (let module M = BLwtCont(LwtStream) in M.test_iteration);
         * create_indexed ~name:"iter/lwt/bstream,cont" ~args (let module M = BLwtCont(LwtBoundedStream) in M.test_iteration);
         * create_indexed ~name:"iter/lwt/wake,cont" ~args (let module M = BLwtCont(LwtWait) in M.test_iteration); *)
        create_indexed ~name:"iter/lwt(stream)/twochan" ~args BLwtStream.test_iteration;

        create_indexed ~name:"iter/mpst-dynamic/lwt(stream)" ~args (let module M = MakeDyn(LwtMonad)(Shmem)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-static/lwt(stream)" ~args (let module M = MakeStatic(LinLwtMonad)(Shmem)() in M.test_iteration);

        (* Interestingly, in Unix pipe, event-based versions are always faster by 2x or more.
         *)
        create_indexed ~name:"iter/lwt/ipc" ~args (let module M = Make_IPC(LwtMonad)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-dynamic/lwt/ipc" ~args (let module M = MakeDyn(LwtMonad)(IPC)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-static/lwt/ipc" ~args (let module M = MakeStatic(LinLwtMonad)(IPC)() in M.test_iteration);

        create_indexed ~name:"iter/ev/ipc" ~args (let module M = Make_IPC(Direct)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-dynamic/ev/ipc" ~args (let module M = MakeDyn(Direct)(IPC)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-static/ev/ipc" ~args (let module M = MakeStatic(LinDirect)(IPC)() in M.test_iteration);
  ])


let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      (test_msgsize @ test_iteration)
