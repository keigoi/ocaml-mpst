open Util
open Ocamlmicro
open Mpstmicro

let test_msgsize =
  let open Core in
  let open Core_bench in
  let args = array_sizes in
  Bench.Test.(
    [
     (* sanity check -- running times for these will not increase *)
     create_indexed ~name:"msgsize/OCaml-lwt/twochan" ~args (let module M = BLwtTwoChan(LwtStream)() in M.test_msgsize);
     create_indexed ~name:"msgsize/OCaml-lwt/cps" ~args (let module M = BLwtCont(LwtMVar)() in M.test_msgsize);

     create_indexed ~name:"msgsize/mpst-dynamic/lwt/shmem,chvec" ~args (let module M = MakeDyn(DynCheckNano)(LwtMonad)(Shmem)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-static/lwt/shmem,chvec" ~args (let module M = MakeStatic(LinLwtMonad)(Shmem)() in M.test_msgsize);

     create_indexed ~name:"msgsize/ocaml-ev/shmem,chvec" ~args @@ (fun i -> Staged.stage (fun () -> BEvent.test_msgsize i));
     create_indexed ~name:"msgsize/ocaml-ev/shmem,untyped" ~args @@ (fun i -> Staged.stage (fun () -> BEventUntyped.test_msgsize i));
     create_indexed ~name:"msgsize/ocaml-ev/shmem,cps" ~args @@ (fun i -> Staged.stage (fun () -> BEventCont.test_msgsize i));

     create_indexed ~name:"msgsize/mpst-ref" ~args @@ (fun i -> Staged.stage (fun () -> BRefImpl.test_msgsize i));

     create_indexed ~name:"msgsize/mpst-dynamic/ev/shmem,chvec" ~args (let module M = MakeDyn(DynCheckNano)(Direct)(Shmem)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-static/ev/shmem,chvec" ~args @@ (let module M = MakeStatic(LinDirect)(Shmem)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-static/ev/shmem,untyped" ~args @@ (let module M = MakeStatic(LinDirect)(Shmem)() in M.test_msgsize);

     (* running times will increase proportional to the array size *)
     create_indexed ~name:"msgsize/lwt/ipc" ~args (let module M = Make_IPC(LwtMonad)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-dynamic/lwt/ipc" ~args (let module M = MakeDyn(DynCheckNano)(LwtMonad)(IPC)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-static/lwt/ipc" ~args (let module M = MakeStatic(LinLwtMonad)(IPC)() in M.test_msgsize);

     create_indexed ~name:"msgsize/ocaml-ev/ipc" ~args (let module M = Make_IPC(Direct)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-dynamic/ev/ipc" ~args (let module M = MakeDyn(DynCheckNano)(Direct)(IPC)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-static/ev/ipc" ~args (let module M = MakeStatic(LinDirect)(IPC)() in M.test_msgsize);
  ])

let one = [1]
let run f = Core.Staged.unstage (f 1)

let test_lwt_ipc =
  let open Core in
  let open Core_bench in
  Bench.Test.(
      [
        create ~name:"lwt_ipc-mpst_dynamic" (let module M = MakeDyn(DynCheckMutex)(LwtMonad)(IPC)() in run M.test_iteration);
        create ~name:"lwt_ipc-mpst_static" (let module M = MakeStatic(LinLwtMonad)(IPC)() in run M.test_iteration);
        create ~name:"lwt_ipc-OCaml_ideal" (let module M = Make_IPC(LwtMonad)() in run M.test_iteration);
      ]
  )
    
let test_ev_ipc =
  let open Core in
  let open Core_bench in
  Bench.Test.(
      [
        create ~name:"ev_ipc-mpst_dynamic" (let module M = MakeDyn(DynCheckMutex)(Direct)(IPC)() in run M.test_iteration);
        create ~name:"ev_ipc-mpst_static" (let module M = MakeStatic(LinDirect)(IPC)() in run M.test_iteration);
        create ~name:"ev_ipc-OCaml_ideal" (let module M = Make_IPC(Direct)() in run M.test_iteration);
      ]
  )

let test_lwt =
  let open Core in
  let open Core_bench in
  Bench.Test.(
      [
        create ~name:"lwt-mpst_dynamic" (let module M = MakeDyn(DynCheckMutex)(LwtMonad)(Shmem)() in run M.test_iteration);
        create ~name:"lwt-mpst_static" (let module M = MakeStatic(LinLwtMonad)(Shmem)() in run M.test_iteration);
        (* create_indexed ~name:"lwt(bstream)-OCaml_cps" ~args (let module M = BLwtCont(LwtBoundedStream)() in run M.test_iteration);
         * create_indexed ~name:"lwt(wake)-OCaml_cps" ~args (let module M = BLwtCont(LwtWait)() in run M.test_iteration); *)
        create ~name:"lwt-OCaml_ideal" (let module M = BLwtTwoChan(LwtStream)() in run M.test_iteration);
      ]
  )

let test_iteration =
  let open Core in
  let open Core_bench in
  Bench.Test.(
      [
        (* Comparions between several versions of ocaml-mpst and OCaml's Event module. This will exhibit overheads in the library.
         * Happily, they are almost negligible when;if:
         * 2) Dynamic checkings are removed (and use static checking from Linocaml instead)
         * Linocaml allocates more memory for closures, it does not affect running times.
         *)
        create ~name:"ev-mpst_dynamic" (let module M = MakeDyn(DynCheckMutex)(Direct)(Shmem)() in run M.test_iteration);
        create ~name:"ev-mpst_static" @@ (let module M = MakeStatic(LinDirect)(Shmem)() in run M.test_iteration);
        create ~name:"ev-OCaml_ideal" @@ run BEvent.test_iteration;

        (* Here- we compare continuation_passing style vs. channel_vector based communication.
         * For Event module- differences are almost negligible (CPS is around 1 % slower)- but apparently CPS allocates
         * more words than two_channel communication.
         *)
        create ~name:"ev-OCaml_cps" @@ run BEventCont.test_iteration;

        (* Lwt is far more faster than Event. Static version is slower; In such a tight loop- cost for monadic closures seems relatively high. *)
        create ~name:"lwt-mpst_dynamic" (let module M = MakeDyn(DynCheckMutex)(LwtMonad)(Shmem)() in run M.test_iteration);
        create ~name:"lwt-mpst_static" (let module M = MakeStatic(LinLwtMonad)(Shmem)() in run M.test_iteration);
        (* create_indexed ~name:"lwt(bstream)-OCaml_cps" ~args (let module M = BLwtCont(LwtBoundedStream)() in run M.test_iteration);
         * create_indexed ~name:"lwt(wake)-OCaml_cps" ~args (let module M = BLwtCont(LwtWait)() in run M.test_iteration); *)
        create ~name:"lwt-OCaml_ideal" (let module M = BLwtTwoChan(LwtStream)() in run M.test_iteration);

        (* For Lwt- CPS is slower than two_channel communication (around 5 %)
         * (It seems that Lwt_mvar is the fastest in CPS_style communication _ as for Lwt version 4.2.1.
         *  Note that MVars are 1_bounded; hence- they are not suitable for chvec MPST implementation)
         *)
        create ~name:"lwt-OCaml_cps" (let module M = BLwtCont(LwtStream)() in run M.test_iteration);

        (* Interestingly- when we use Unix pipe- event_based versions are always faster by 2x or more.
         * Also- static (monadic) versions are always faster; it seems that closures are GC'ed during i;o.
         *)
        create ~name:"ev_ipc-mpst_dynamic" (let module M = MakeDyn(DynCheckMutex)(Direct)(IPC)() in run M.test_iteration);
        create ~name:"ev_ipc-mpst_static" (let module M = MakeStatic(LinDirect)(IPC)() in run M.test_iteration);
        create ~name:"ev_ipc-OCaml_ideal" (let module M = Make_IPC(Direct)() in run M.test_iteration);

        create ~name:"lwt_ipc-mpst_dynamic" (let module M = MakeDyn(DynCheckMutex)(LwtMonad)(IPC)() in run M.test_iteration);
        create ~name:"lwt_ipc-mpst_static" (let module M = MakeStatic(LinLwtMonad)(IPC)() in run M.test_iteration);
        create ~name:"lwt_ipc-OCaml_ideal" (let module M = Make_IPC(LwtMonad)() in run M.test_iteration);

        create ~name:"lwt_mvar-OCaml_ideal" (let module M = BLwtTwoChan(LwtMVar)() in run M.test_iteration);
        create ~name:"lwt_mvar-OCaml_cps" (let module M = BLwtCont(LwtMVar)() in run M.test_iteration);

        (* Chcek why it exactly is slow. Closures around endpoints incur a huge cost (~ 20 %) in a tight loop.
         * Nano_mutex does not cause much slow down.  *)
        create ~name:"lwt_nodyncheck-mpst_dynamic" (let module M = MakeDyn(NoDynCheckWithClosure)(LwtMonad)(Shmem)() in run M.test_iteration);
        create ~name:"lwt_nodyncheck-noclosure-mpst_dynamic" (let module M = MakeDyn(NoDynCheck)(LwtMonad)(Shmem)() in run M.test_iteration);

        (* channel vector by hand *)
        create ~name:"lwt-OCaml_overhead_closure" @@ run BLwtChannelVectorManualDyncheckClosure.test_iteration;
        create ~name:"lwt-OCaml_overhead" @@ run BLwtChannelVectorManual.test_iteration;
        (* less closure- no polyvar wrap (but have object wrap- with extra closures on reception) *)
        create ~name:"lwt-OCaml_less_overhead" @@ run BLwtChannelVectorManualLessWrap.test_iteration;
        (* less closure- no polyvar wrap (but have object wrap) *)
        create ~name:"lwt-OCaml_less_overhead1_5" @@ run BLwtChannelVectorManualLessWrap1_5.test_iteration;
        (* no object wrap (but have polyvar wrap) *)
        create ~name:"lwt-OCaml_less_overhead2" @@ run BLwtChannelVectorManualLessWrap2.test_iteration;
        (* almost same as twochan *)
        create ~name:"lwt-OCaml_less_overhead3" @@ run BLwtChannelVectorManualLessWrap3.test_iteration;

        (* (\* For Event-based one, the overhead for dynamic checking is negligible. Closures has some cost (less than 5 percent) *\)
         * create ~name:"mpst-static/ev" @@ (let module M = MakeStatic(LinDirect)(Shmem)() in run M.test_iteration);
         * create ~name:"mpst-dynamic/ev" (let module M = MakeDyn(DynCheckMutex)(Direct)(Shmem)() in run M.test_iteration);
         * create ~name:"mpst-dynamic/ev(nodyncheck)" (let module M = MakeDyn(NoDynCheckWithClosure)(Direct)(Shmem)() in run M.test_iteration);
         * create ~name:"mpst-dynamic/ev(nodyncheck,noclosure)" (let module M = MakeDyn(NoDynCheck)(Direct)(Shmem)() in run M.test_iteration);
         * create ~name:"OCaml-ev/twochan" BEvent.test_iteration; *)
  ])


let () =
  Lwt_engine.set (new Lwt_engine.libev ~backend:Lwt_engine.Ev_backend.poll ())
  
let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      test_lwt_ipc
