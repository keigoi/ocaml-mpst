open Core_bench.Bench
open Core_bench.Bench.Test
open Bench_util.Util
open Chameleons_body

(* let nums_threads = [200; 100; 50; 10] *)
(* let nums_threads = [3] *)
(* let nums_threads = [1000; 400; 200; 50; 10; 3; 10] *)
let nums_threads = [5000; 1000; 100; 50; 3]
(* let nums_threads = [2000; 1500] *)

module NanoMutexReuseEP =
  Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheck(Bench_util.Dyncheck_nanomutex.NanoMutexFlag))
module NanoMutexFreshEP =
  Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheckClosure(Bench_util.Dyncheck_nanomutex.NanoMutexFlag))
module PosixMutexReuseEP =
  Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheck(Mpst.LinFlag.PosixMutexFlag))
module PosixMutexFreshEP =
  Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheckClosure(Mpst.LinFlag.PosixMutexFlag))

module NoCheckEP =
  Mpst.Endpoints.Make(Mpst.Lin.NoCheck)
              
let run ~name t =
  create_indexed ~args:nums_threads ~name t

let test_lwt = [
  run ~name:"lwt_dynamic" (let module M = MakeDyn(NanoMutexReuseEP)(LwtMonad)(Shmem)() in M.runtest);
  run ~name:"lwt_dynamic_fresh" (let module M = MakeDyn(NanoMutexFreshEP)(LwtMonad)(Shmem)() in M.runtest);
  
  run ~name:"lwt_dynamic_untyped" (let module M = MakeDyn(NanoMutexReuseEP)(LwtMonad)(Untyped)() in M.runtest);
  (* run ~name:"lwt_dynamic_fresh_untyped" (let module M = MakeDyn(NanoMutexFreshEP)(LwtMonad)(Untyped)() in M.runtest); *)

  (* run ~name:"lwt_static" (let module M = MakeStatic(LinLwtMonad)(Shmem)() in M.runtest);
   * run ~name:"lwt_static_untyped" (let module M = MakeStatic(LinLwtMonad)(Untyped)() in M.runtest); *)
  ]

(* too slow *)
(* let test_ev = [
 *   run ~name:"ev_dynamic" (let module M = MakeDyn (NanoMutexReuseEP)(Direct)(Shmem)() in M.runtest);
 *   run ~name:"ev_dynamic_fresh" (let module M = MakeDyn (NanoMutexFreshEP)(Direct)(Shmem)() in M.runtest);
 *   run ~name:"ev_dynamic_nocheck" (let module M = MakeDyn (NoCheckEP)(Direct)(Shmem)() in M.runtest);
 *   
 *   run ~name:"ev_dynamic_untyped" (let module M = MakeDyn (NanoMutexReuseEP)(Direct)(Untyped)() in M.runtest);
 *   run ~name:"ev_dynamic_fresh_untyped" (let module M = MakeDyn (NanoMutexFreshEP)(Direct)(Untyped)() in M.runtest);
 *   run ~name:"ev_dynamic_nocheck_untyped" (let module M = MakeDyn (NoCheckEP)(Direct)(Untyped)() in M.runtest);
 * 
 *   (\* run ~name:"ev_static" @@ (let module M = MakeStatic(LinDirect)(Shmem)() in M.runtest);
 *    * run ~name:"ev_static_untyped" @@ (let module M = MakeStatic(LinDirect)(Untyped)() in M.runtest); *\)
 *   ] *)

let filter x = List.filter (fun x -> x<=200) nums_threads

let test_ipc = [
      create_indexed
        ~args:(filter nums_threads)
        ~name:"lwt_ipc_dynamic" (let module M = MakeDyn(NanoMutexReuseEP)(LwtMonad)(IPC)() in M.runtest);
      run ~name:"ipc_dynamic" (let module M = MakeDyn(NanoMutexReuseEP)(Direct)(IPC)() in M.runtest);
      (* run ~name:"ipc_static" (let module M = MakeStatic(LinDirect)(IPC)() in M.runtest); *)
    ]

let test_all =
    test_lwt @ test_ipc

let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      (* test_all *)
      test_ipc
      (* test_lwt *)

