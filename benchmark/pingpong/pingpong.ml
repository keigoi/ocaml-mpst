open Core_bench.Bench
open Core_bench.Bench.Test
open Bench_util
open Bench_util.Util
open Pingpong_body

(* array size parameters for ipc payloads *)
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
