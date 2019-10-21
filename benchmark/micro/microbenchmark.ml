open Core_bench.Bench
open Core_bench.Bench.Test
open Bench_util
open Bench_util.Util
open Mpstmicro

let args = array_sizes

module ReuseNanoMutexEP =
  Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheck(Bench_util.Dyncheck_nanomutex.NanoMutexFlag))
(* module ReusePosixMutexEP =
 *   Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheck(Mpst.LinFlag.PosixMutexFlag))
 * module FreshNanoMutexEP =
 *   Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheckClosure(Bench_util.Dyncheck_nanomutex.NanoMutexFlag))
 * module FreshPosixMutexEP =
 *   Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheckClosure(Mpst.LinFlag.PosixMutexFlag)) *)

module FreshNoCheckEP =
  Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheckClosure(Mpst.LinFlag.PosixMutexFlag))
module ReuseNoCheckEP =
  Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheckClosure(Mpst.LinFlag.PosixMutexFlag))
module NoCheckEP =
  Mpst.Endpoints.Make(Mpst.Lin.NoCheck)

module EP = ReuseNanoMutexEP (* faster than default Mpst.EP *)
          
let run f = Core.Staged.unstage (f (List.nth array_sizes 0))

let chvec_counts_for_bare_ocaml_check = [1]
(* let chvec_counts = [1;10;100] *)
(* let array_sizes = [List.nth array_sizes (List.length array_sizes -1)] *)

let test_ev = [
    (* Comparions between several versions of ocaml-mpst and OCaml's Event module. This will exhibit overheads in the library.
     * Happily, they are almost negligible when;if:
     * 2) Dynamic checkings are removed (and use static checking from Linocaml instead)
     * Linocaml allocates more memory for closures, it does not affect running times.
     *)
    Test.create ~name:"ev-mpst_dynamic" (let module M = MakeDyn(EP)(Direct)(Shmem)() in run M.runtest);
    Test.create ~name:"ev-mpst_static" @@ (let module M = MakeStatic(LinDirect)(Shmem)() in run M.runtest);
    Test.create ~name:"ev-mpst_ref" @@ run BRefImpl.runtest;
  ]

let test_lwt = [
    (* Lwt is far more faster than Event. Static version is slower; In such a tight loop- cost for monadic closures seems relatively high. *)
    Test.create ~name:"lwt-mpst_dynamic" (let module M = MakeDyn(EP)(LwtMonad)(Shmem)() in run M.runtest);
    Test.create ~name:"lwt-mpst_static" (let module M = MakeStatic(LinLwtMonad)(Shmem)() in run M.runtest);
  ]

let test_lwt_ipc = [
    create_indexed ~args ~name:"lwt_ipc-mpst_dynamic" (let module M = MakeDyn(EP)(LwtMonad)(IPC)() in M.runtest);
    create_indexed ~args ~name:"lwt_ipc-mpst_static" (let module M = MakeStatic(LinLwtMonad)(IPC)() in M.runtest);
  ]

let test_ipc = [
    (* Interestingly, when we use Unix pipe, event-based versions are always faster by 2x or more.
     * Also, static (monadic) versions are always faster; it seems that closures are GC'ed during i/o.
     *)
    create_indexed ~args ~name:"ipc-mpst_dynamic" (let module M = MakeDyn(EP)(Direct)(IPC)() in M.runtest);
    create_indexed ~args ~name:"ipc-mpst_static" (let module M = MakeStatic(LinDirect)(IPC)() in M.runtest);
  ]


let test_ep_closures = [
    create ~name:"mpst-dynamic/ev(nodyncheck,fresh)" (let module M = MakeDyn(FreshNoCheckEP)(Direct)(Shmem)() in run M.runtest);
    create ~name:"mpst-dynamic/ev(nodyncheck,reuse)" (let module M = MakeDyn(ReuseNoCheckEP)(Direct)(Shmem)() in run M.runtest);
    create ~name:"mpst-dynamic/ev(nodyncheck)" (let module M = MakeDyn(NoCheckEP)(Direct)(Shmem)() in run M.runtest);
    create ~name:"lwt_opt_nodyncheck-mpst_dynamic" (let module M = MakeDyn(NoCheckEP)(LwtMonad)(Shmem)() in run M.runtest);
  ]
  

let test_iteration =
    test_ev @ test_lwt @ test_ipc @ test_lwt_ipc

let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      test_iteration

open Ocamlmicro

let args = chvec_counts_for_bare_ocaml_check

let test_bare_ocaml = [
    create ~name:"ev-OCaml_ideal" @@ run BEvent.runtest;

    (* Here, we compare continuation_passing style vs. channel_vector based communication.
     * For Event module, differences are almost negligible (CPS is around 1 % slower), but apparently CPS allocates
     * more words than two_channel communication.
     *)
    create ~name:"ev-OCaml_cps" @@ run BEventCont.runtest;

    create_indexed ~args ~name:"lwt-OCaml_ideal" (let module M = BLwtTwoChan(LwtStream)() in M.runtest);

    create_indexed ~args ~name:"lwt_opt-OCaml_ideal" (let module M = BLwtTwoChan(LwtOptStream)() in M.runtest);

    (* For Lwt, CPS is slower than two_channel communication (around 5 %)
     * (It seems that Lwt_mvar is the fastest in CPS_style communication _ as for Lwt version 4.2.1.
     *  Note that MVars are 1_bounded; hence- they are not suitable for chvec MPST implementation)
     *)

    create ~name:"lwt-OCaml_cps" (let module M = BLwtCont(LwtStream)() in run M.runtest);
    create ~name:"lwt_opt-OCaml_cps" (let module M = BLwtCont(LwtOptStream)() in run M.runtest);
    create_indexed ~args ~name:"lwt_mvar-OCaml_ideal" (let module M = BLwtTwoChan(LwtMVar)() in M.runtest);
    create ~name:"lwt_bstream-OCaml_ideal" (let module M = BLwtTwoChan(LwtBoundedStream)() in run M.runtest);
    create ~name:"lwt_wake-OCaml_ideal" (let module M = BLwtTwoChan(LwtWait)() in run M.runtest);
    create ~name:"lwt_mvar-OCaml_cps" (let module M = BLwtCont(LwtMVar)() in run M.runtest);
    create ~name:"lwt_bstream-OCaml_cps" (let module M = BLwtCont(LwtBoundedStream)() in run M.runtest);
    create ~name:"lwt_wake-OCaml_cps" (let module M = BLwtCont(LwtWait)() in run M.runtest);

    (* ipc *)
    create_indexed ~args ~name:"lwt_ipc-OCaml_ideal" (let module M = Make_IPC(LwtMonad)() in M.runtest);
    create_indexed ~args ~name:"ipc-OCaml_ideal" (let module M = Make_IPC(Direct)() in M.runtest);

    (* Chcek why it exactly is slow. Closures around endpoints incur a huge cost (~ 20 %) in a tight loop.
     * Nano_mutex does not cause much slow down.  *)
    (* channel vector by hand *)
    create ~name:"lwt_opt-OCaml_overhead" (let module M = BLwtChannelVectorManual(LwtOptStream) in run M.runtest);
    (* less closure- no polyvar wrap (but have object wrap- with extra closures on reception) *)
    create ~name:"lwt_opt-OCaml_less_overhead" (let module M = BLwtChannelVectorManualLessWrap(LwtOptStream) in run M.runtest);
    (* less closure- no polyvar wrap (but have object wrap) *)
    create ~name:"lwt_opt-OCaml_less_overhead1_5" (let module M = BLwtChannelVectorManualLessWrap1_5(LwtOptStream) in run M.runtest);
    (* no object wrap (but have polyvar wrap) *)
    create ~name:"lwt_opt-OCaml_less_overhead2" (let module M = BLwtChannelVectorManualLessWrap2(LwtOptStream) in run M.runtest);
    (* almost same as twochan *)
    create ~name:"lwt_opt-OCaml_less_overhead3" (let module M = BLwtChannelVectorManualLessWrap3(LwtOptStream) in run M.runtest);
  ]
