open Core_bench.Bench
open Core_bench.Bench.Test
open Bench_util.Util
open Chameleons_body

let nums_threads = [5000; 2500; 1000; 500; 100; 75; 50; 25; 3]

module NanoMutexReuseEP =
  Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheck(Bench_util.Dyncheck_nanomutex.NanoMutexFlag))
module NanoMutexFreshEP =
  Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheckClosure(Bench_util.Dyncheck_nanomutex.NanoMutexFlag))

let run ?(args=nums_threads) ~name t =
  create_indexed ~args ~name t

let test_lwt = [
  run ~name:"lwt_dynamic" (let module M = MakeDyn(NanoMutexReuseEP)(LwtMonad)(Shmem)() in M.runtest);
  run ~name:"lwt_dynamic_fresh" (let module M = MakeDyn(NanoMutexFreshEP)(LwtMonad)(Shmem)() in M.runtest);

  run ~name:"lwt_dynamic_untyped" (let module M = MakeDyn(NanoMutexReuseEP)(LwtMonad)(Untyped)() in M.runtest);
  run ~name:"lwt_dynamic_fresh_untyped" (let module M = MakeDyn(NanoMutexFreshEP)(LwtMonad)(Untyped)() in M.runtest);

  run ~name:"lwt_static" (let module M = MakeStatic(LinLwtMonad)(Shmem)() in M.runtest);
  run ~name:"lwt_static_untyped" (let module M = MakeStatic(LinLwtMonad)(Untyped)() in M.runtest);
  ]

(* too slow since they are synchronous *)
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

let test_all =
    test_lwt

let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      test_all
