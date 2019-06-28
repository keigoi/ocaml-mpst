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
     create_indexed ~name:"msgsize/ocaml-lwt/shmem,chvec" ~args BLwtStream.test_msgsize;
     create_indexed ~name:"msgsize/ocaml-lwt/shmem,cont" ~args (let module M = BLwtCont(LwtMVar)() in M.test_msgsize);

     create_indexed ~name:"msgsize/mpst-dynamic/lwt/shmem,chvec" ~args (let module M = MakeDyn(DynCheckNano)(LwtMonad)(Shmem)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-dynamic/lwt/shmem,untyped" ~args (let module M = MakeDyn(DynCheckNano)(LwtMonad)(Untyped)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-static/lwt/shmem,chvec" ~args (let module M = MakeStatic(LinLwtMonad)(Shmem)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-static/lwt/shmen,untyped" ~args (let module M = MakeStatic(LinLwtMonad)(Untyped)() in M.test_msgsize);

     create_indexed ~name:"msgsize/ocaml-ev/shmem,chvec" ~args @@ (fun i -> Staged.stage (fun () -> BEvent.test_msgsize i));
     create_indexed ~name:"msgsize/ocaml-ev/shmem,untyped" ~args @@ (fun i -> Staged.stage (fun () -> BEventUntyped.test_msgsize i));
     create_indexed ~name:"msgsize/ocaml-ev/shmem,cont" ~args @@ (fun i -> Staged.stage (fun () -> BEventCont.test_msgsize i));

     create_indexed ~name:"msgsize/mpst-ref" ~args @@ (fun i -> Staged.stage (fun () -> BRefImpl.test_msgsize i));

     create_indexed ~name:"msgsize/mpst-dynamic/ev/shmem,chvec" ~args (let module M = MakeDyn(DynCheckNano)(Direct)(Shmem)() in M.test_msgsize);
     create_indexed ~name:"msgsize/mpst-dynamic/ev/shmem,untyped" ~args @@ (let module M = MakeDyn(DynCheckNano)(Direct)(Untyped)() in M.test_msgsize);
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
        create_indexed ~name:"iter/ocaml-ev/cont" ~args BEventCont.test_iteration;
        create_indexed ~name:"iter/ocaml-ev/twochan" ~args BEvent.test_iteration;

        (* Comparions between the bare Event module with ocaml-mpst. this will exhibits overheads in the library.
         * Happily, they are almost negligible when/if:
         * 1) Dynamic linearity checking uses quicker mutex implementation (e.g. Core.Nano_mutex), or
         * 2) Dynamic checkings are removed (and use static checking from Linocaml instead)
         * Linocaml allocates more memory for closures, it does not affect running times.
         *)
        create_indexed ~name:"iter/mpst-ref/ev/shmem" ~args BRefImpl.test_iteration;
        create_indexed ~name:"iter/mpst-dynamic/ev/shmem" ~args (let module M = MakeDyn(DynCheckNano)(Direct)(Shmem)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-dynamic/ev/shmem(nodyncheck)" ~args (let module M = MakeDyn(NoDynCheck)(Direct)(Shmem)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-static/ev/shmem" ~args @@ (let module M = MakeStatic(LinDirect)(Shmem)() in M.test_iteration);

        (* Lwt is far more faster than Event.
         * Again, we compare CPS with two-channel communication.
         * (It seems that Lwt_mvar is the fastest in CPS-style communication - as for Lwt version 4.2.1.
         *  Note that MVars are 1-bounded; hence, they are not suitable for MPST implementation)
         * Still, CPS version is slower than two-channel ver (around 5 %)
         *)
        create_indexed ~name:"iter/ocaml-lwt(mvar)/cont" ~args (let module M = BLwtCont(LwtMVar)() in M.test_iteration);
        create_indexed ~name:"iter/ocaml-lwt(stream),cont" ~args (let module M = BLwtCont(LwtStream)() in M.test_iteration);
        (* create_indexed ~name:"iter/ocaml-lwt(bstream),cont" ~args (let module M = BLwtCont(LwtBoundedStream)() in M.test_iteration);
         * create_indexed ~name:"iter/ocaml-lwt(wake),cont" ~args (let module M = BLwtCont(LwtWait)() in M.test_iteration); *)
        create_indexed ~name:"iter/ocaml-lwt(stream)/twochan" ~args BLwtStream.test_iteration;

        create_indexed ~name:"iter/mpst-dynamic/lwt(stream)" ~args (let module M = MakeDyn(DynCheckNano)(LwtMonad)(Shmem)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-dynamic/lwt(stream,nodyncheck)" ~args (let module M = MakeDyn(NoDynCheck)(LwtMonad)(Shmem)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-static/lwt(stream)" ~args (let module M = MakeStatic(LinLwtMonad)(Shmem)() in M.test_iteration);

        (* Interestingly, in Unix pipe, event-based versions are always faster by 2x or more.
         *)
        create_indexed ~name:"iter/ocaml-lwt/ipc" ~args (let module M = Make_IPC(LwtMonad)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-dynamic/lwt/ipc" ~args (let module M = MakeDyn(DynCheckNano)(LwtMonad)(IPC)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-static/lwt/ipc" ~args (let module M = MakeStatic(LinLwtMonad)(IPC)() in M.test_iteration);

        create_indexed ~name:"iter/ev/ipc" ~args (let module M = Make_IPC(Direct)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-dynamic/ev/ipc" ~args (let module M = MakeDyn(DynCheckNano)(Direct)(IPC)() in M.test_iteration);
        create_indexed ~name:"iter/mpst-static/ev/ipc" ~args (let module M = MakeStatic(LinDirect)(IPC)() in M.test_iteration);
  ])


let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      test_iteration
