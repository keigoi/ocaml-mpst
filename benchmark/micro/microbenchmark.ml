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

let last = [5000]
let test_iteration =
  let open Core in
  let open Core_bench in
  let args = iteration_counts in
  Bench.Test.(
      [
        (* Comparions between several versions of ocaml-mpst and OCaml's Event module. This will exhibit overheads in the library.
         * Happily, they are almost negligible when/if:
         * 2) Dynamic checkings are removed (and use static checking from Linocaml instead)
         * Linocaml allocates more memory for closures, it does not affect running times.
         *)
        create_indexed ~name:"iter/mpst-dynamic/ev" ~args:last (let module M = MakeDyn(DynCheckNano)(Direct)(Shmem)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-static/ev" ~args:last @@ (let module M = MakeStatic(LinDirect)(Shmem)() in M.test_iteration);
        create_indexed ~name:"iter/OCaml-ev/twochan" ~args:last BEvent.test_iteration;

        (* Lwt is far more faster than Event. Static version is slower; In such a tight loop, cost for monadic closures seems relatively high. *)
        create_indexed ~name:"iter/mpst-dynamic/lwt(stream)" ~args:last (let module M = MakeDyn(DynCheckNano)(LwtMonad)(Shmem)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-static/lwt(stream)" ~args:last (let module M = MakeStatic(LinLwtMonad)(Shmem)() in M.test_iteration);
        (* create_indexed ~name:"iter/OCaml-lwt(bstream),cps" ~args (let module M = BLwtCont(LwtBoundedStream)() in M.test_iteration);
         * create_indexed ~name:"iter/OCaml-lwt(wake),cps" ~args (let module M = BLwtCont(LwtWait)() in M.test_iteration); *)
        create_indexed ~name:"iter/OCaml-lwt/twochan(stream)" ~args:last (let module M = BLwtTwoChan(LwtStream)() in M.test_iteration);

        (* Interestingly, when we use Unix pipe, event-based versions are always faster by 2x or more.
         * Also, static (monadic) versions are always faster; it seems that closures are GC'ed during i/o.
         *)
        create_indexed ~name:"iter/mpst-dynamic/ev/ipc" ~args:last (let module M = MakeDyn(DynCheckNano)(Direct)(IPC)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-static/ev/ipc" ~args:last (let module M = MakeStatic(LinDirect)(IPC)() in M.test_iteration);
        create_indexed ~name:"iter/OCaml-ev/ipc" ~args:last (let module M = Make_IPC(Direct)() in M.test_iteration);

        create_indexed ~name:"iter/mpst-dynamic/lwt/ipc" ~args:last (let module M = MakeDyn(DynCheckNano)(LwtMonad)(IPC)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-static/lwt/ipc" ~args:last (let module M = MakeStatic(LinLwtMonad)(IPC)() in M.test_iteration);
        create_indexed ~name:"iter/OCaml-lwt/ipc" ~args:last (let module M = Make_IPC(LwtMonad)() in M.test_iteration);

        (* Here, we compare continuation-passing style vs. channel-vector based communication.
         * For Event module, differences are almost negligible (CPS is around 1 % slower), but apparently CPS allocates
         * more words than two-channel communication.
         *)
        create_indexed ~name:"iter/OCaml-ev/twochan" ~args BEvent.test_iteration;
        create_indexed ~name:"iter/OCaml-ev/cps" ~args BEventCont.test_iteration;

        (* For Lwt, CPS is slower than two-channel communication (around 5 %)
         * (It seems that Lwt_mvar is the fastest in CPS-style communication - as for Lwt version 4.2.1.
         *  Note that MVars are 1-bounded; hence, they are not suitable for chvec MPST implementation)
         *)
        create_indexed ~name:"iter/OCaml-lwt/twochan(stream)" ~args (let module M = BLwtTwoChan(LwtStream)() in M.test_iteration);
        create_indexed ~name:"iter/OCaml-lwt/cps(stream)" ~args (let module M = BLwtCont(LwtStream)() in M.test_iteration);

        create_indexed ~name:"iter/OCaml-lwt/twochan(mvar)" ~args (let module M = BLwtTwoChan(LwtMVar)() in M.test_iteration);
        create_indexed ~name:"iter/OCaml-lwt/cps(mvar)" ~args (let module M = BLwtCont(LwtMVar)() in M.test_iteration);

        (* Sanity check. running time is propotional to iteration count. *)
        (* create_indexed ~name:"iter/mpst-ref/ev" ~args:last BRefImpl.test_iteration; *)
        create_indexed ~name:"iter/mpst-dynamic/ev" ~args (let module M = MakeDyn(DynCheckNano)(Direct)(Shmem)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-static/ev" ~args @@ (let module M = MakeStatic(LinDirect)(Shmem)() in M.test_iteration);

        create_indexed ~name:"iter/mpst-dynamic/lwt(stream)" ~args (let module M = MakeDyn(DynCheckNano)(LwtMonad)(Shmem)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-static/lwt(stream)" ~args (let module M = MakeStatic(LinLwtMonad)(Shmem)() in M.test_iteration);

        create_indexed ~name:"iter/mpst-dynamic/ev/ipc" ~args (let module M = MakeDyn(DynCheckNano)(Direct)(IPC)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-static/ev/ipc" ~args (let module M = MakeStatic(LinDirect)(IPC)() in M.test_iteration);

        create_indexed ~name:"iter/mpst-dynamic/lwt/ipc" ~args (let module M = MakeDyn(DynCheckNano)(LwtMonad)(IPC)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-static/lwt/ipc" ~args (let module M = MakeStatic(LinLwtMonad)(IPC)() in M.test_iteration);

        (* Chcek why it exactly is slow. Closures around endpoints incur a huge cost (~ 20 %) in a tight loop.
         * Nano_mutex does not cause much slow down.  *)
        create_indexed ~name:"iter/mpst-static/lwt(stream)" ~args:last (let module M = MakeStatic(LinLwtMonad)(Shmem)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-dynamic/lwt" ~args:last (let module M = MakeDyn(DynCheckNano)(LwtMonad)(Shmem)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-dynamic/lwt(nodyncheck)" ~args:last (let module M = MakeDyn(NoDynCheckWithClosure)(LwtMonad)(Shmem)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-dynamic/lwt(nodyncheck,noclosure)" ~args:last (let module M = MakeDyn(NoDynCheck)(LwtMonad)(Shmem)() in M.test_iteration);

        (* channel vector by hand *)
        create_indexed ~name:"iter/OCaml-lwt/chvec_byhand(stream),closure" ~args:last BLwtChannelVectorManualDyncheckClosure.test_iteration;
        create_indexed ~name:"iter/OCaml-lwt/chvec_byhand(stream)" ~args:last BLwtChannelVectorManual.test_iteration;
        (* less closure, no polyvar wrap (but have object wrap, with extra closures on reception) *)
        create_indexed ~name:"iter/OCaml-lwt/chvec_byhand_less_wrap(stream)" ~args:last BLwtChannelVectorManualLessWrap.test_iteration;
        (* less closure, no polyvar wrap (but have object wrap) *)
        create_indexed ~name:"iter/OCaml-lwt/chvec_byhand_less_wrap1_5(stream)" ~args:last BLwtChannelVectorManualLessWrap1_5.test_iteration;
        (* no object wrap (but have polyvar wrap) *)
        create_indexed ~name:"iter/OCaml-lwt/chvec_byhand_less_wrap2(stream)" ~args:last BLwtChannelVectorManualLessWrap2.test_iteration;
        (* almost same as twochan *)
        create_indexed ~name:"iter/OCaml-lwt/chvec_byhand_less_wrap3(stream)" ~args:last BLwtChannelVectorManualLessWrap3.test_iteration;
        create_indexed ~name:"iter/OCaml-lwt/twochan(stream)" ~args:last (let module M = BLwtTwoChan(LwtStream)() in M.test_iteration);

        (* (\* For Event-based one, the overhead for dynamic checking is negligible. Closures has some cost (less than 5 percent) *\)
         * create_indexed ~name:"iter/mpst-static/ev" ~args:last @@ (let module M = MakeStatic(LinDirect)(Shmem)() in M.test_iteration);
         * create_indexed ~name:"iter/mpst-dynamic/ev" ~args:last (let module M = MakeDyn(DynCheckNano)(Direct)(Shmem)() in M.test_iteration);
         * create_indexed ~name:"iter/mpst-dynamic/ev(nodyncheck)" ~args:last (let module M = MakeDyn(NoDynCheckWithClosure)(Direct)(Shmem)() in M.test_iteration);
         * create_indexed ~name:"iter/mpst-dynamic/ev(nodyncheck,noclosure)" ~args:last (let module M = MakeDyn(NoDynCheck)(Direct)(Shmem)() in M.test_iteration);
         * create_indexed ~name:"iter/OCaml-ev/twochan" ~args:last BEvent.test_iteration; *)
  ])


let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      test_iteration
