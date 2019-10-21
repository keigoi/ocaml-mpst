open Core_bench.Bench
open Core_bench.Bench.Test
open Bench_util
open Bench_util.Util
open Pingpong_body

(* array size parameters for ipc payloads *)
let args = array_sizes

module NanoMutexReuseEP =
  Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheck(Bench_util.Dyncheck_nanomutex.NanoMutexFlag))
module NanoMutexFreshEP =
  Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheckClosure(Bench_util.Dyncheck_nanomutex.NanoMutexFlag))
module PosixMutexReuseEP =
  Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheck(Mpst.LinFlag.PosixMutexFlag))
module PosixMutexFreshEP =
  Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheckClosure(Mpst.LinFlag.PosixMutexFlag))

module NoCheckFreshEP =
  Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheckClosure(Mpst.LinFlag.PosixMutexFlag))
module NoCheckReuseEP =
  Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheckClosure(Mpst.LinFlag.PosixMutexFlag))
module NoCheckEP =
  Mpst.Endpoints.Make(Mpst.Lin.NoCheck)

module EP = NanoMutexReuseEP (* faster than default Mpst.EP *)
(* module EP = PosixMutexReuseEP (\* same as default Mpst.EP *\) *)
          
let run f = Core.Staged.unstage (f (List.nth array_sizes 0))

let test_ev = [
    (* Comparions between several versions of ocaml-mpst and OCaml's Event module. This will exhibit overheads in the library.
     * Happily, they are almost negligible when;if:
     * 2) Dynamic checkings are removed (and use static checking from Linocaml instead)
     * Linocaml allocates more memory for closures, it does not affect running times.
     *)
    create ~name:"ev_dynamic" (let module M = MakeDyn(NanoMutexReuseEP)(Direct)(Shmem)() in run M.runtest);
    create ~name:"ev_dynamic_posixmutex" (let module M = MakeDyn(PosixMutexReuseEP)(Direct)(Shmem)() in run M.runtest);
    create ~name:"ev_dynamic_nocheck" (let module M = MakeDyn(NoCheckEP)(Direct)(Shmem)() in run M.runtest);
    create ~name:"ev_static" @@ (let module M = MakeStatic(LinDirect)(Shmem)() in run M.runtest);
    create ~name:"ev_ref" @@ run BRefImpl.runtest;
  ]

let test_lwt = [
    (* Lwt is far more faster than Event. Static version is slower; In such a tight loop- cost for monadic closures seems relatively high. *)
    create ~name:"lwt_dynamic" (let module M = MakeDyn(NanoMutexReuseEP)(LwtMonad)(Shmem)() in run M.runtest);
    create ~name:"lwt_dynamic_posixmutex" (let module M = MakeDyn(PosixMutexReuseEP)(LwtMonad)(Shmem)() in run M.runtest);
    create ~name:"lwt_dynamic_freshnanomutex" (let module M = MakeDyn(NanoMutexFreshEP)(LwtMonad)(Shmem)() in run M.runtest);
    create ~name:"lwt_dynamic_nocheck" (let module M = MakeDyn(NoCheckEP)(LwtMonad)(Shmem)() in run M.runtest);
    create ~name:"lwt_static" (let module M = MakeStatic(LinLwtMonad)(Shmem)() in run M.runtest);
  ]

let test_lwt_ipc = [
    create_indexed ~args ~name:"lwt_ipc_dynamic" (let module M = MakeDyn(EP)(LwtMonad)(IPC)() in M.runtest);
    create_indexed ~args ~name:"lwt_ipc_static" (let module M = MakeStatic(LinLwtMonad)(IPC)() in M.runtest);
  ]

let test_ipc = [
    (* Interestingly, when we use Unix pipe, event-based versions are always faster by 2x or more.
     * Also, static (monadic) versions are always faster; it seems that closures are GC'ed during i/o.
     *)
    create_indexed ~args ~name:"ipc_dynamic" (let module M = MakeDyn(EP)(Direct)(IPC)() in M.runtest);
    create_indexed ~args ~name:"ipc_dynamic_posixmutex" (let module M = MakeDyn(PosixMutexReuseEP)(Direct)(IPC)() in M.runtest);
    create_indexed ~args ~name:"ipc_static" (let module M = MakeStatic(LinDirect)(IPC)() in M.runtest);
  ]

let test_all =
    test_ev @ test_lwt @ test_ipc @ test_lwt_ipc

let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      test_all
