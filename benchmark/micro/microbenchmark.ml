open Util
open Ocamlmicro
open Mpstmicro
let () =
  Lwt_io.set_default_buffer_size (1000000 * 16)
let array_sizes = [1000000]

let one = [1]
let run f = Core.Staged.unstage (f 1)

let test_lwt =
  let open Core in
  let open Core_bench in
  Bench.Test.(
    [
        (* Lwt is far more faster than Event. Static version is slower; In such a tight loop- cost for monadic closures seems relatively high. *)
        create ~name:"lwt-mpst_dynamic" (let module M = MakeDyn(DynCheckMutex)(LwtMonad)(Shmem)() in run M.runtest);
        create ~name:"lwt-mpst_static" (let module M = MakeStatic(LinLwtMonad)(Shmem)() in run M.runtest);
        create ~name:"lwt-OCaml_ideal" (let module M = BLwtTwoChan(LwtStream)() in run M.runtest);
        (* For Lwt, CPS is slower than two_channel communication (around 5 %)
         * (It seems that Lwt_mvar is the fastest in CPS_style communication _ as for Lwt version 4.2.1.
         *  Note that MVars are 1_bounded; hence- they are not suitable for chvec MPST implementation)
         *)
        create ~name:"lwt-OCaml_cps" (let module M = BLwtCont(LwtStream)() in run M.runtest);
        create ~name:"lwt(bstream)-OCaml_cps" (let module M = BLwtCont(LwtBoundedStream)() in run M.runtest);
        (* create ~name:"lwt(wake)-OCaml_cps" (let module M = BLwtCont(LwtWait)() in run M.runtest); *)

        create ~name:"lwt_mvar-OCaml_ideal" (let module M = BLwtTwoChan(LwtMVar)() in run M.runtest);
        create ~name:"lwt_mvar-OCaml_cps" (let module M = BLwtCont(LwtMVar)() in run M.runtest);

        (* Chcek why it exactly is slow. Closures around endpoints incur a huge cost (~ 20 %) in a tight loop.
         * Nano_mutex does not cause much slow down.  *)
        create ~name:"lwt_nodyncheck-mpst_dynamic" (let module M = MakeDyn(NoDynCheckWithClosure)(LwtMonad)(Shmem)() in run M.runtest);
        create ~name:"lwt_nodyncheck-noclosure-mpst_dynamic" (let module M = MakeDyn(NoDynCheck)(LwtMonad)(Shmem)() in run M.runtest);
        (* channel vector by hand *)
        create ~name:"lwt-OCaml_overhead_closure" @@ run BLwtChannelVectorManualDyncheckClosure.runtest;
        create ~name:"lwt-OCaml_overhead" @@ run BLwtChannelVectorManual.runtest;
        (* less closure- no polyvar wrap (but have object wrap- with extra closures on reception) *)
        create ~name:"lwt-OCaml_less_overhead" @@ run BLwtChannelVectorManualLessWrap.runtest;
        (* less closure- no polyvar wrap (but have object wrap) *)
        create ~name:"lwt-OCaml_less_overhead1_5" @@ run BLwtChannelVectorManualLessWrap1_5.runtest;
        (* no object wrap (but have polyvar wrap) *)
        create ~name:"lwt-OCaml_less_overhead2" @@ run BLwtChannelVectorManualLessWrap2.runtest;
        (* almost same as twochan *)
        create ~name:"lwt-OCaml_less_overhead3" @@ run BLwtChannelVectorManualLessWrap3.runtest;
      ]
  )

let test_ev =
  let open Core in
  let open Core_bench in
  Bench.Test.(
      [
        (* Comparions between several versions of ocaml-mpst and OCaml's Event module. This will exhibit overheads in the library.
         * Happily, they are almost negligible when;if:
         * 2) Dynamic checkings are removed (and use static checking from Linocaml instead)
         * Linocaml allocates more memory for closures, it does not affect running times.
         *)
        create ~name:"ev-mpst_dynamic" (let module M = MakeDyn(DynCheckMutex)(Direct)(Shmem)() in run M.runtest);
        create ~name:"ev-mpst_static" @@ (let module M = MakeStatic(LinDirect)(Shmem)() in run M.runtest);
        create ~name:"ev-mpst_ref" @@ run BRefImpl.runtest;
        create ~name:"ev-OCaml_ideal" @@ run BEvent.runtest;

        (* Here, we compare continuation_passing style vs. channel_vector based communication.
         * For Event module, differences are almost negligible (CPS is around 1 % slower), but apparently CPS allocates
         * more words than two_channel communication.
         *)
        create ~name:"ev-OCaml_cps" @@ run BEventCont.runtest;
      ])

let test_lwt_ipc =
  let args = array_sizes in
  let open Core in
  let open Core_bench in
  Bench.Test.(
      [
        create_indexed ~args ~name:"lwt_ipc-mpst_dynamic" (let module M = MakeDyn(DynCheckMutex)(LwtMonad)(IPC)() in M.runtest);
        create_indexed ~args ~name:"lwt_ipc-mpst_static" (let module M = MakeStatic(LinLwtMonad)(IPC)() in M.runtest);
        create_indexed ~args ~name:"lwt_ipc-OCaml_ideal" (let module M = Make_IPC(LwtMonad)() in M.runtest);
      ]
  )
    
let test_ev_ipc =
  let args = array_sizes in
  let open Core in
  let open Core_bench in
  Bench.Test.(
      [
        (* Interestingly- when we use Unix pipe, event_based versions are always faster by 2x or more.
         * Also, static (monadic) versions are always faster; it seems that closures are GC'ed during i;o.
         *)
        create_indexed ~args ~name:"ev_ipc-mpst_dynamic" (let module M = MakeDyn(DynCheckMutex)(Direct)(IPC)() in M.runtest);
        create_indexed ~args ~name:"ev_ipc-mpst_static" (let module M = MakeStatic(LinDirect)(IPC)() in M.runtest);
        create_indexed ~args ~name:"ev_ipc-OCaml_ideal" (let module M = Make_IPC(Direct)() in M.runtest);
        (* create ~name:"mpst-dynamic/ev(nodyncheck)" (let module M = MakeDyn(NoDynCheckWithClosure)(Direct)(Shmem)() in run M.runtest);
         * create ~name:"mpst-dynamic/ev(nodyncheck,noclosure)" (let module M = MakeDyn(NoDynCheck)(Direct)(Shmem)() in run M.runtest); *)
      ]
  )

let test_iteration =
  let open Core in
  let open Core_bench in
  Bench.Test.(
    test_ev @ test_lwt @ test_ev_ipc @ test_lwt_ipc
  )

(* let () =
 *   Lwt_engine.set (new Lwt_engine.libev ~backend:Lwt_engine.Ev_backend.poll ()) *)
  
let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      test_iteration
