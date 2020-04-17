open Concur_shims
open Core_bench.Bench
open Core_bench.Bench.Test
open Bench_util.Util
open Chameleons_body

let nums_threads = [2500; 1000; 500; 100; 75; 50; 25; 3]

let run ?(args=nums_threads) ~name t =
  create_indexed ~args ~name t

let prefix =
  if IO.is_direct then
    "ev_"
  else
    "lwt_"

let test = [
  run ~name:(prefix^"dynamic") (let module M = MakeDyn(Shmem)() in M.runtest);
  run ~name:(prefix^"dynamic_untyped") (let module M = MakeDyn(Untyped)() in M.runtest);
  run ~name:(prefix^"static") (let module M = MakeStatic(Shmem)() in M.runtest);
  run ~name:(prefix^"static_untyped") (let module M = MakeStatic(Untyped)() in M.runtest);
  ]

let test_all =
    test

let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      test_all
