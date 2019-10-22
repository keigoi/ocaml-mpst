open Core_bench.Bench
open Core_bench.Bench.Test
open Bench_util
open Bench_util.Util
open Npingbody

let nping_num = [750; 500; 100; 20; 10; 5; 1]

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
  Test.create_indexed ~args:nping_num ~name t


let test_lwt = [
    run ~name:"lwt_dynamic" (let module M = MakeDyn(NanoMutexReuseEP)(LwtMonad)(Shmem)() in M.runtest);
    run ~name:"lwt_dynamic_fresh" (let module M = MakeDyn(NanoMutexFreshEP)(LwtMonad)(Shmem)() in M.runtest);
    run ~name:"lwt_dynamic_nocheck" (let module M = MakeDyn(NoCheckEP)(LwtMonad)(Shmem)() in M.runtest);

    run ~name:"lwt_static" (let module M = MakeStatic(LinLwtMonad)(Shmem)() in M.runtest);

    run ~name:"lwt_dynamic_untyped" (let module M = MakeDyn(NanoMutexReuseEP)(LwtMonad)(Untyped)() in M.runtest);
    run ~name:"lwt_dynamic_fresh_untyped" (let module M = MakeDyn(NanoMutexFreshEP)(LwtMonad)(Untyped)() in M.runtest);
    run ~name:"lwt_dynamic_nocheck_untyped" (let module M = MakeDyn(NoCheckEP)(LwtMonad)(Untyped)() in M.runtest);
    run ~name:"lwt_static_untyped" (let module M = MakeStatic(LinLwtMonad)(Untyped)() in M.runtest);

  ]

let test_ev =      [
    (* chvecs *)
    run ~name:"ev_dynamic" (let module M = MakeDyn (NanoMutexReuseEP)(Direct)(Shmem)() in M.runtest);
    run ~name:"ev_dynamic_fresh" (let module M = MakeDyn (NanoMutexFreshEP)(Direct)(Shmem)() in M.runtest);
    run ~name:"ev_dynamic_posix" (let module M = MakeDyn (PosixMutexReuseEP)(Direct)(Shmem)() in M.runtest);
    run ~name:"ev_dynamic_nocheck" (let module M = MakeDyn (NoCheckEP)(Direct)(Shmem)() in M.runtest);
    run ~name:"ev_static" @@ (let module M = MakeStatic(LinDirect)(Shmem)() in M.runtest);

    (* untyped *)
    run ~name:"ev_dynamic_untyped" (let module M = MakeDyn (NanoMutexReuseEP)(Direct)(Untyped)() in M.runtest);
    run ~name:"ev_dynamic_fresh_untyped" (let module M = MakeDyn (NanoMutexFreshEP)(Direct)(Untyped)() in M.runtest);
    run ~name:"ev_dynamic_posix_untyped" (let module M = MakeDyn (PosixMutexReuseEP)(Direct)(Untyped)() in M.runtest);
    run ~name:"ev_dynamic_posix_fresh_untyped" (let module M = MakeDyn (PosixMutexFreshEP)(Direct)(Untyped)() in M.runtest);
    run ~name:"ev_dynamic_nocheck_untyped" (let module M = MakeDyn(NoCheckEP)(Direct)(Untyped)() in M.runtest);
    run ~name:"ev_dynamic_static_untyped" @@ (let module M = MakeStatic(LinDirect)(Untyped)() in M.runtest);
    run ~name:"ev_static_untyped" @@ (let module M = MakeStatic(LinDirect)(Shmem)() in M.runtest);
  ]

let test_ipc = [
    run ~name:"ipc_dynamic" (let module M = MakeDyn(NanoMutexReuseEP)(Direct)(IPC)() in M.runtest);
    run ~name:"ipc_static" (let module M = MakeStatic(LinDirect)(IPC)() in M.runtest);
    run ~name:"ipc_dynamic_nocheck" (let module M = MakeDyn(NoCheckEP)(Direct)(IPC)() in M.runtest);
  ]


let test_iteration =
    test_lwt @ test_ev @ test_ipc

let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      test_iteration
