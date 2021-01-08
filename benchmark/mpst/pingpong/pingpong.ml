open Concur_shims
open Core_bench.Bench
open Core_bench.Bench.Test
open Bench_util
open Bench_util.Util
open Pingpong_body

(* array size parameters for ipc payloads *)
let args = array_sizes

let run f = Core.Staged.unstage (f (List.nth array_sizes 0))

let prefix =
  if IO.is_direct then
    "ev_"
  else
    "lwt_"

let test = [
    (* Comparions between several versions of ocaml-mpst and OCaml's Event module. This will exhibit overheads in the library.
     * Happily, they are almost negligible when;if:
     * 2) Dynamic checkings are removed (and use static checking from Linocaml instead)
     * Linocaml allocates more memory for closures, it does not affect running times.
     *)
    create_indexed ~args:[1;10;100] ~name:(prefix^"dynamic") (let module M = MakeDyn(Shmem)() in M.runtest);
    (* create_indexed ~args:[1;10;100] ~name:(prefix^"dynamic_untyped") (let module M = MakeDyn(Untyped)() in M.runtest); *)
    create_indexed ~args:[1;10;100] ~name:(prefix^"static") @@ (let module M = MakeStatic(Shmem)() in M.runtest);
    (* create_indexed ~args:[1;10;100] ~name:(prefix^"static_untyped") @@ (let module M = MakeStatic(Untyped)() in M.runtest); *)
  ]

let test_ipc =
  if IO.is_direct then
    [
      (* Interestingly, when we use Unix pipe, event-based versions are always faster by 2x or more.
       * Also, static (monadic) versions are always faster; it seems that closures are GC'ed during i/o.
      *)
      create_indexed ~args ~name:"ipc_dynamic" (let module M = MakeDyn(IPC)() in M.runtest);
      create_indexed ~args ~name:"ipc_static" (let module M = MakeStatic(IPC)() in M.runtest);
    ]
  else
    []

let test_all =
  test @ test_ipc

let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      test_all
