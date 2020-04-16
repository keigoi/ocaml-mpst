(* ulimit -n 65536 *)
open Core_bench.Bench
open Core_bench.Bench.Test
open Bench_util.Util
open Chameleons_body

let nums_threads = [5000; 2500; 1000; 500; 100; 75; 50; 25; 3]

let run ?(args=nums_threads) ~name t =
  create_indexed ~args ~name t

let test_pipes = [
    run ~name:"ipc_dynamic" (let module M = MakeDyn(IPC)() in M.runtest);
    run ~name:"ipc_static" (let module M = MakeStatic(IPC)() in M.runtest);
    run ~name:"lwt_ipc_dynamic" (let module M = MakeDyn(IPC)() in M.runtest);
    run ~name:"ipc_static" (let module M = MakeStatic(IPC)() in M.runtest);
  ]

let test_all =
    test_pipes

let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      test_pipes
