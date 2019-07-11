open Core_bench.Bench
open Util
open Ocamlmicro
open Mpstmicro

let run f = Core.Staged.unstage (f (List.nth array_sizes 0))

let chvec_counts = [1;10;100]
(* let array_sizes = [List.nth array_sizes (List.length array_sizes -1)] *)

let lwt_mpst_dynamic =
  Test.create ~name:"lwt-mpst_dynamic" (let module M = MakeDyn(Mpst.EP)(LwtMonad)(Shmem)() in run M.runtest)

let lwt_mpst_static =
  Test.create ~name:"lwt-mpst_static" (let module M = MakeStatic(LinLwtMonad)(Shmem)() in run M.runtest)

let ev_mpst_dynamic =
  Test.create ~name:"ev-mpst_dynamic" (let module M = MakeDyn (Mpst.EP)(Direct)(Shmem)() in run M.runtest)

let ev_mpst_static =
  Test.create ~name:"ev-mpst_static" @@ (let module M = MakeStatic(LinDirect)(Shmem)() in run M.runtest)
  
let ev_mpst_ref =
  Test.create ~name:"ev-mpst_ref" @@ run BRefImpl.runtest

let lwt_ideal =
  Test.create_indexed ~args:chvec_counts ~name:"lwt-OCaml_ideal" (let module M = BLwtTwoChan(LwtStream)() in M.runtest)

let lwt_cps =
  Test.create ~name:"lwt-OCaml_cps" (let module M = BLwtCont(LwtStream)() in run M.runtest)

let lwt_ideal_opt =
  Test.create_indexed ~args:chvec_counts ~name:"lwt_opt-OCaml_ideal" (let module M = BLwtTwoChan(LwtOptStream)() in M.runtest)

let lwt_cps_opt =
  Test.create ~name:"lwt_opt-OCaml_cps" (let module M = BLwtCont(LwtOptStream)() in run M.runtest)

let ev_cps =    
  Test.create ~name:"ev-OCaml_cps" @@ run BEventCont.runtest

let ev_ideal =
  Test.create ~name:"ev-OCaml_ideal" @@ run BEvent.runtest

let lwt_mvar_ideal =
  Test.create_indexed ~args:chvec_counts ~name:"lwt_mvar-OCaml_ideal" (let module M = BLwtTwoChan(LwtMVar)() in M.runtest)  
let lwt_bstream_ideal =
  Test.create ~name:"lwt_bstream-OCaml_ideal" (let module M = BLwtTwoChan(LwtBoundedStream)() in run M.runtest)
let lwt_wake_ideal =
  Test.create ~name:"lwt_wake-OCaml_ideal" (let module M = BLwtTwoChan(LwtWait)() in run M.runtest)
let lwt_mvar_cps =      
  Test.create ~name:"lwt_mvar-OCaml_cps" (let module M = BLwtCont(LwtMVar)() in run M.runtest)
let lwt_bstream_cps =
  Test.create ~name:"lwt_bstream-OCaml_cps" (let module M = BLwtCont(LwtBoundedStream)() in run M.runtest)
let lwt_wake_cps =
  Test.create ~name:"lwt_wake-OCaml_cps" (let module M = BLwtCont(LwtWait)() in run M.runtest)

let test_lwt =
  Core_bench.Bench.Test.(
    [
        (* Lwt is far more faster than Event. Static version is slower; In such a tight loop- cost for monadic closures seems relatively high. *)
        lwt_mpst_dynamic;
        lwt_mpst_static;
        lwt_ideal;
        lwt_ideal_opt;
        (* For Lwt, CPS is slower than two_channel communication (around 5 %)
         * (It seems that Lwt_mvar is the fastest in CPS_style communication _ as for Lwt version 4.2.1.
         *  Note that MVars are 1_bounded; hence- they are not suitable for chvec MPST implementation)
         *)
        (* lwt_cps;
         * lwt_cps_opt;
         * lwt_mvar_ideal;
         * lwt_mvar_cps;
         * lwt_bstream_cps;
         * (\* lwt_wake_cps; *\) *)

        (* Chcek why it exactly is slow. Closures around endpoints incur a huge cost (~ 20 %) in a tight loop.
         * Nano_mutex does not cause much slow down.  *)
        (* create ~name:"lwt_nodyncheck-mpst_dynamic" (let module M = MakeDyn(NoDynCheckWithClosure)(LwtMonad)(Shmem)() in run M.runtest);
         * create ~name:"lwt_nodyncheck-noclosure-mpst_dynamic" (let module M = MakeDyn(NoDynCheck)(LwtMonad)(Shmem)() in run M.runtest);
         * (\* channel vector by hand *\)
         * create ~name:"lwt-OCaml_overhead_closure" @@ run BLwtChannelVectorManualDyncheckClosure.runtest;
         * create ~name:"lwt-OCaml_overhead" @@ run BLwtChannelVectorManual.runtest;
         * (\* less closure- no polyvar wrap (but have object wrap- with extra closures on reception) *\)
         * create ~name:"lwt-OCaml_less_overhead" @@ run BLwtChannelVectorManualLessWrap.runtest;
         * (\* less closure- no polyvar wrap (but have object wrap) *\)
         * create ~name:"lwt-OCaml_less_overhead1_5" @@ run BLwtChannelVectorManualLessWrap1_5.runtest;
         * (\* no object wrap (but have polyvar wrap) *\)
         * create ~name:"lwt-OCaml_less_overhead2" @@ run BLwtChannelVectorManualLessWrap2.runtest;
         * (\* almost same as twochan *\)
         * create ~name:"lwt-OCaml_less_overhead3" @@ run BLwtChannelVectorManualLessWrap3.runtest; *)
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
        ev_mpst_dynamic;
        ev_mpst_static;
        ev_mpst_ref;
        ev_ideal;

        (* Here, we compare continuation_passing style vs. channel_vector based communication.
         * For Event module, differences are almost negligible (CPS is around 1 % slower), but apparently CPS allocates
         * more words than two_channel communication.
         *)
        ev_cps
      ])

let test_lwt_ipc =
  let args = array_sizes in
  let open Core in
  let open Core_bench in
  Bench.Test.(
      [
        create_indexed ~args ~name:"lwt_ipc-mpst_dynamic" (let module M = MakeDyn(Mpst.EP)(LwtMonad)(IPC)() in M.runtest);
        create_indexed ~args ~name:"lwt_ipc-mpst_static" (let module M = MakeStatic(LinLwtMonad)(IPC)() in M.runtest);
        create_indexed ~args ~name:"lwt_ipc-OCaml_ideal" (let module M = Make_IPC(LwtMonad)() in M.runtest);
      ]
  )
    
let test_ipc =
  let args = array_sizes in
  let open Core in
  let open Core_bench in
  Bench.Test.(
      [
        (* Interestingly- when we use Unix pipe, event_based versions are always faster by 2x or more.
         * Also, static (monadic) versions are always faster; it seems that closures are GC'ed during i;o.
         *)
        create_indexed ~args ~name:"ipc-mpst_dynamic" (let module M = MakeDyn(Mpst.EP)(Direct)(IPC)() in M.runtest);
        create_indexed ~args ~name:"ipc-mpst_static" (let module M = MakeStatic(LinDirect)(IPC)() in M.runtest);
        create_indexed ~args ~name:"ipc-OCaml_ideal" (let module M = Make_IPC(Direct)() in M.runtest);
        (* create ~name:"mpst-dynamic/ev(nodyncheck)" (let module M = MakeDyn(NoDynCheckWithClosure)(Direct)(Shmem)() in run M.runtest);
         * create ~name:"mpst-dynamic/ev(nodyncheck,noclosure)" (let module M = MakeDyn(NoDynCheck)(Direct)(Shmem)() in run M.runtest); *)
      ]
  )

let test_iteration =
    test_ev @ test_lwt @ test_ipc @ test_lwt_ipc

(* let () =
 *   Lwt_engine.set (new Lwt_engine.libev ~backend:Lwt_engine.Ev_backend.poll ()) *)
  
let () =
  (* let gc = Gc.get() in
   * Gc.set { gc with Gc.minor_heap_size = gc.Gc.minor_heap_size * 2 }; *)
  Core.Command.run @@
    Core_bench.Bench.make_command
      test_iteration
      
