open Core_bench.Bench
open Bench_util.Util
open Npingbody

(* let run f = Core.Staged.unstage (f (List.nth array_sizes 0)) *)

(* let nping_num = [1; 100; 10000] *)
(* let nping_num = [100; 500; 2000; 4000] *)
let nping_num = [4000; 2000; 500; 100]
(* let nping_num = [1] *)

let run ~name t =
  Test.create_indexed ~args:nping_num ~name t

let lwt_mpst_dynamic =
  run ~name:"lwt-mpst_dynamic" (let module M = MakeDyn(Mpst.EP)(LwtMonad)(Shmem)() in M.runtest)

let lwt_mpst_dynamic_untyped =
  run ~name:"lwt-mpst_dynamic_untyped" (let module M = MakeDyn(Mpst.EP)(LwtMonad)(Untyped)() in M.runtest)

let lwt_mpst_static =
  run ~name:"lwt-mpst_static" (let module M = MakeStatic(LinLwtMonad)(Shmem)() in M.runtest)

let lwt_mpst_static_untyped =
  run ~name:"lwt-mpst_static" (let module M = MakeStatic(LinLwtMonad)(Untyped)() in M.runtest)

let ev_mpst_dynamic =
  run ~name:"ev-mpst_dynamic" (let module M = MakeDyn (Mpst.EP)(Direct)(Shmem)() in M.runtest)

let ev_mpst_dynamic_untyped =
  run ~name:"ev-mpst_dynamic_untyped" (let module M = MakeDyn (Mpst.EP)(Direct)(Untyped)() in M.runtest)

let ev_mpst_static =
  run ~name:"ev-mpst_static" @@ (let module M = MakeStatic(LinDirect)(Shmem)() in M.runtest)

let ev_mpst_static =
  run ~name:"ev-mpst_static" @@ (let module M = MakeStatic(LinDirect)(Shmem)() in M.runtest)

let ev_mpst_static_untyped =
  run ~name:"ev-mpst_static" @@ (let module M = MakeStatic(LinDirect)(Untyped)() in M.runtest)

let test_lwt =
  Core_bench.Bench.Test.(
    [
        lwt_mpst_dynamic;
        lwt_mpst_dynamic_untyped;
        lwt_mpst_static;
        lwt_mpst_static_untyped;
      ]
  )

let test_ev =
  let open Core in
  let open Core_bench in
  Bench.Test.(
      [
        ev_mpst_dynamic;
        ev_mpst_dynamic_untyped;
        ev_mpst_static;
        ev_mpst_static_untyped;
      ])

let test_ipc =
  let open Core in
  let open Core_bench in
  Bench.Test.(
      [
        run ~name:"ipc-mpst_dynamic" (let module M = MakeDyn(Mpst.EP)(Direct)(IPC)() in M.runtest);
        run ~name:"ipc-mpst_static" (let module M = MakeStatic(LinDirect)(IPC)() in M.runtest);
      ]
  )

let test_iteration =
    test_lwt @ test_ev @ test_ipc

let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      (* test_ev *)
      (* test_lwt *)
      test_iteration
