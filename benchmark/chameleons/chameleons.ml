open Core_bench.Bench
open Bench_util.Util
open Chameleons_body

(* let nums_threads = [10; 7; 5; 3] *)
let nums_threads = [3]

module FastEP = Mpst.Endpoints.Make(Mpst.Lin.NoCheck)
module NoReuseMutexEP = Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheckClosure(Mpst.LinFlag.PosixMutexFlag))
module NanoMutexEP = Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheck(Bench_util.Dyncheck_nanomutex.NanoMutexFlag))
module NoReuseNanoMutexEP = Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheckClosure(Bench_util.Dyncheck_nanomutex.NanoMutexFlag))
              
let run ~name t =
  Test.create_indexed ~args:nums_threads ~name t

let ev_mpst_dynamic =
  run ~name:"ev-mpst_dynamic" (let module M = MakeDyn (Mpst.EP)(Direct)(Shmem)() in M.runtest)

let ev_mpst_dynamic_untyped =
  run ~name:"ev-mpst_dynamic_untyped" (let module M = MakeDyn (Mpst.EP)(Direct)(Untyped)() in M.runtest)

let ev_mpst_noreuse =
  run ~name:"ev-mpst_noreuse" (let module M = MakeDyn (NoReuseMutexEP)(Direct)(Shmem)() in M.runtest)

let ev_mpst_noreuse_untyped =
  run ~name:"ev-mpst_noreuse_untyped" (let module M = MakeDyn (NoReuseMutexEP)(Direct)(Untyped)() in M.runtest)

let ev_mpst_nanomutex =
  run ~name:"ev-mpst_nano" (let module M = MakeDyn (NanoMutexEP)(Direct)(Shmem)() in M.runtest)

let ev_mpst_nanomutex_untyped =
  run ~name:"ev-mpst_nano_untyped" (let module M = MakeDyn (NanoMutexEP)(Direct)(Untyped)() in M.runtest)

let ev_mpst_nanomutex_noreuse =
  run ~name:"ev-mpst_nano-noreuse" (let module M = MakeDyn (NoReuseNanoMutexEP)(Direct)(Shmem)() in M.runtest)

let ev_mpst_nanomutex_noreuse_untyped =
  run ~name:"ev-mpst_nano-noreuse_untyped" (let module M = MakeDyn (NoReuseNanoMutexEP)(Direct)(Untyped)() in M.runtest)

let ev_mpst_static =
  run ~name:"ev-mpst_static" @@ (let module M = MakeStatic(LinDirect)(Shmem)() in M.runtest)

let ev_mpst_static_untyped =
  run ~name:"ev-mpst_static_untyped" @@ (let module M = MakeStatic(LinDirect)(Untyped)() in M.runtest)

let ev_mpst_faster =
  run ~name:"ev-mpst_faster" (let module M = MakeDyn (FastEP)(Direct)(Shmem)() in M.runtest)

let ev_mpst_faster_untyped =
  run ~name:"ev-mpst_faster_untyped" (let module M = MakeDyn (FastEP)(Direct)(Untyped)() in M.runtest)

let test_ev =
  let open Core in
  let open Core_bench in
  Bench.Test.(
      [
        ev_mpst_dynamic;
        (* ev_mpst_noreuse; *)
        (* ev_mpst_nanomutex; *)
        (* ev_mpst_nanomutex_noreuse; *)
        (* ev_mpst_static;
         * ev_mpst_faster;
         * ev_mpst_dynamic_untyped;
         * ev_mpst_noreuse_untyped;
         * ev_mpst_nanomutex_untyped;
         * ev_mpst_nanomutex_noreuse_untyped;
         * ev_mpst_static_untyped;
         * ev_mpst_faster_untyped; *)
      ])

let test_ipc =
  let open Core in
  let open Core_bench in
  Bench.Test.(
      [
        run ~name:"ipc-mpst_dynamic" (let module M = MakeDyn(Mpst.EP)(Direct)(IPC)() in M.runtest);
        run ~name:"ipc-mpst_faster" (let module M = MakeDyn(FastEP)(Direct)(IPC)() in M.runtest);
        run ~name:"ipc-mpst_static" (let module M = MakeStatic(LinDirect)(IPC)() in M.runtest);
      ]
  )

let test_iteration =
    test_ev @ test_ipc

let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      test_ev
      (* test_lwt *)
      (* test_ipc *)
      (* test_iteration *)
