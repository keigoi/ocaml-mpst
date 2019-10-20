open Core_bench.Bench
open Bench_util.Util
open Chameleons_body

let nums_threads = [200; 100; 50; 10]
(* let nums_threads = [1000; 400; 200; 50; 3] *)
(* let nums_threads = [5000; 1000; 100; 50; 3] *)
(* let nums_threads = [2000; 1500] *)

module FastEP = Mpst.Endpoints.Make(Mpst.Lin.NoCheck)
module NoReuseMutexEP = Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheckClosure(Mpst.LinFlag.PosixMutexFlag))
module NanoMutexEP = Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheck(Bench_util.Dyncheck_nanomutex.NanoMutexFlag))
module NoReuseNanoMutexEP = Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheckClosure(Bench_util.Dyncheck_nanomutex.NanoMutexFlag))
              
let run ~name t =
  Test.create_indexed ~args:nums_threads ~name t

let lwt_mpst_dynamic =
  run ~name:"lwt-mpst_dynamic" (let module M = MakeDyn(Mpst.EP)(LwtMonad)(Shmem)() in M.runtest)

let lwt_mpst_dynamic_untyped =
  run ~name:"lwt-mpst_dynamic_untyped" (let module M = MakeDyn(Mpst.EP)(LwtMonad)(Untyped)() in M.runtest)

let lwt_mpst_static =
  run ~name:"lwt-mpst_static" (let module M = MakeStatic(LinLwtMonad)(Shmem)() in M.runtest)

let lwt_mpst_static_untyped =
  run ~name:"lwt-mpst_static_untyped" (let module M = MakeStatic(LinLwtMonad)(Untyped)() in M.runtest)

let lwt_mpst_faster =
  run ~name:"lwt-mpst_faster" (let module M = MakeDyn(FastEP)(LwtMonad)(Shmem)() in M.runtest)

let lwt_mpst_faster_untyped =
  run ~name:"lwt-mpst_faster_untyped" (let module M = MakeDyn(FastEP)(LwtMonad)(Untyped)() in M.runtest)
  
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
        ev_mpst_noreuse;
        ev_mpst_nanomutex;
        ev_mpst_nanomutex_noreuse;
        (* ev_mpst_static; *)
        ev_mpst_faster;
        (* ev_mpst_dynamic_untyped;
         * ev_mpst_noreuse_untyped;
         * ev_mpst_nanomutex_untyped;
         * ev_mpst_nanomutex_noreuse_untyped;
         * ev_mpst_static_untyped;
         * ev_mpst_faster_untyped; *)
      ])

let test_lwt =
  Core_bench.Bench.Test.(
    [
        lwt_mpst_dynamic_untyped;
        lwt_mpst_dynamic;
        (* lwt_mpst_static;
         * lwt_mpst_static_untyped; *)
        (* lwt_mpst_faster; *)
        (* lwt_mpst_faster_untyped; *)
      ]
  )

let filter x = List.filter (fun x -> x<=200) nums_threads

let test_ipc =
  let open Core in
  let open Core_bench in
  Bench.Test.(
    [
      Test.create_indexed
        ~args:(filter nums_threads)
        ~name:"lwt_ipc-mpst_dynamic" (let module M = MakeDyn(Mpst.EP)(LwtMonad)(IPC)() in M.runtest);
      run ~name:"ipc-mpst_dynamic" (let module M = MakeDyn(Mpst.EP)(Direct)(IPC)() in M.runtest);
      (* run ~name:"ipc-mpst_static" (let module M = MakeStatic(LinDirect)(IPC)() in M.runtest); *)
    ]
  )

let test_iteration =
    test_ipc @ test_lwt

let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      test_lwt
      (* test_ipc *)
      (* test_iteration *)
