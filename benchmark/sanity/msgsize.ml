open Mpst_base
let (>>=) = Lwt.(>>=)

module Bench(M:S.MPST) = struct
  open M
  open Global
  open Session
  open Util

  let a = {role=`A; lens=Fst}
  let b = {role=`B; lens=Next Fst}

  let finish = one @@ one @@ nil

  let mkglobal () =
    let rec g =
      lazy begin
          (b --> a) msg @@
            choice_at a left_or_right
              (a, (a --> b) left @@
                    finish)
              (a, (a --> b) right @@
                    loop g)
        end
    in
    Lazy.force g

  let tA cnt s =
    let rec loop i s =
      receive `B s >>= fun (`msg((),s)) ->
      if i = 0 then begin
          let s = send `B (fun x->x#left) () s in
          close s;
          Lwt.return ()
        end else begin
          let s = send `B (fun x->x#right) () s in
          loop (i-1) s
        end
    in
    loop cnt s

  let tB s =
    let rec loop s =
      let s = send `A (fun x->x#msg) () s in
      receive `A s >>= function
      | `left((),s) ->
         close s;
         Lwt.return ()
      | `right((),s) ->
         loop s
    in
    loop s

end

module Bench_shmem = Bench(Mpst_shmem)
module Bench_implicit_ipc = Bench(Mpst_implicit.IPC)
module Bench_implicit_lwt = Bench(Mpst_implicit.Lwt)
                          
let run_shmem cnt = Core.Staged.stage @@ fun () ->
  let open Bench_shmem in
  let open Mpst_shmem.Global in
  let g = mkglobal () in
  let sa = get_sess a g in
  let sb = get_sess b g in
  Lwt_main.run (Lwt.join [tA cnt sa; tB sb])

module type IMPLICIT = sig
  include S.MPST
  type conn
  val assign :
    (Obj.t * (Obj.t * conn list list) list) list ->
    'a Global.slots lazy_t -> 'a Global.slots lazy_t
  val mkpipes :
    [>  ] list ->
    't Global.slots lazy_t -> (Obj.t * (Obj.t * conn list list) list) list
  val pipes : [>  ] list -> 'a Global.slots lazy_t -> 'a Global.slots lazy_t
end
                     

let run_implicit_lwt0 (module M:IMPLICIT) cnt =
  let module Bench = Bench(M) in
  let open Bench in
  let open M in
  let open Global in
  let pipes = mkpipes [`A;`B] (mkglobal ()) in
  Core.Staged.stage @@ fun () ->
  let g = assign pipes (mkglobal ()) in
  let sa = get_sess a g in
  let sb = get_sess b g in
  Lwt_main.run (Lwt.join [tA cnt sa; tB sb])

let run_implicit_lwt_nodynchk cnt =
  run_implicit_lwt0 (module Mpst_implicit.LwtNoDynCheck) cnt

let run_implicit_lwt cnt =
  run_implicit_lwt0 (module Mpst_implicit.Lwt) cnt

let run_implicit_pipe_nodynchk cnt =
  run_implicit_lwt0 (module Mpst_implicit.IPCNoDynCheck) cnt

let run_implicit_pipe cnt =
  run_implicit_lwt0 (module Mpst_implicit.IPC) cnt

let run_implicit_fork (module M:IMPLICIT) cnt =
  let module Bench = Bench(M) in
  let open Bench in
  let open M in
  let open Global in
  let pipes = mkpipes [`A;`B] (mkglobal ()) in
  Core.Staged.stage @@ fun () ->
  let g = assign pipes (mkglobal ()) in
  let sa = get_sess a g in
  let sb = get_sess b g in
  fork (fun () -> Lwt_main.run (tB sb));
  Lwt_main.run (tA cnt sa)

let run_implicit_ipc_nodynchk cnt =
  run_implicit_fork (module Mpst_implicit.IPCNoDynCheck) cnt

let run_implicit_ipc cnt =
  run_implicit_fork (module Mpst_implicit.IPC) cnt
  
(* let counts = [1000;2000;3000;5000;10000] *)
(* let counts = [1000;10000] *)
(* let counts = [10000] *)
let counts = [1000]

(* https://blog.janestreet.com/core_bench-micro-benchmarking-for-ocaml/ *)
let () =
  let open Core in
  let open Core_bench in
  Command.run
    (Bench.make_command [
         (* Bench.Test.create_indexed ~name:"shmem" ~args:counts run_shmem; *)
         Bench.Test.create_indexed ~name:"implicit/lwt(nodyn)" ~args:counts run_implicit_lwt_nodynchk;
         Bench.Test.create_indexed ~name:"implicit/lwt" ~args:counts run_implicit_lwt;
         (* Bench.Test.create_indexed ~name:"implicit/pipe(nodyn)" ~args:counts run_implicit_pipe_nodynchk; *)
         (* Bench.Test.create_indexed ~name:"implicit/pipe" ~args:counts run_implicit_pipe; *)
         (* Bench.Test.create_indexed ~name:"implicit/IPCpipe(nodyn)" ~args:counts run_implicit_ipc_nodynchk; *)
         (* Bench.Test.create_indexed ~name:"implicit/IPCpipe" ~args:counts run_implicit_ipc; *)
    ])
