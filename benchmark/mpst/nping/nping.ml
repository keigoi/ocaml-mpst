open Concur_shims
open Core_bench.Bench
open Core_bench.Bench.Test
open Bench_util
open Bench_util.Util
open Npingbody

let nping_num = [ 750; 500; 100; 20; 10; 5; 1 ]
let run ~name t = Test.create_indexed ~args:nping_num ~name t

let test =
  if IO.is_direct then []
  else
    [
      run ~name:"lwt_dynamic"
        (let module M = MakeDyn (Shmem) () in
        M.runtest);
      run ~name:"lwt_static"
        (let module M = MakeStatic (Shmem) () in
        M.runtest);
      run ~name:"lwt_dynamic_single"
        (let module M = MakeDyn (Untyped) () in
        M.runtest);
      run ~name:"lwt_static_single"
        (let module M = MakeStatic (Untyped) () in
        M.runtest);
    ]

(* let test_ipc =
 *   if IO.is_direct then
 *     [
 *       run ~name:"ipc_dynamic" (let module M = MakeDyn(IPC)() in M.runtest);
 *       run ~name:"ipc_static" (let module M = MakeStatic(IPC)() in M.runtest);
 *       run ~name:"ipc_dynamic_nocheck" (let module M = MakeDyn(IPC)() in M.runtest);
 *     ]
 *   else
 *     [] *)

let () = Core.Command.run @@ Core_bench.Bench.make_command test
