(* simple ring protocol *)
open Mpst_shmem.Session
open Mpst_shmem.Global
open Mpst_shmem.Util

let (>>=) = Lwt.(>>=)

let a : ([`A],_,_,_,_) role = {role=`A; lens=Fst}
let b : ([`B],_,_,_,_) role = {role=`B; lens=Next Fst}
let c : ([`C],_,_,_,_) role = {role=`C; lens=Next (Next Fst)}

let finish = one @@ one @@ one @@ nil

(* A global protocol between A, B, and C *)
let g = (a --> b) msg @@
        (b --> c) msg @@
        (c --> a) msg @@
        finish

let sa, sb, sc =
  get_sess a g, get_sess b g, get_sess c g

let a : [`A] = `A
let b : [`B] = `B
let c : [`C] = `C

(* participant A *)
let tA : unit Lwt.t =
  let open Lwt in
  let sa = send b (fun x->x#msg) () sa in
  receive c sa >>= fun (`msg((), sa)) ->
  close sa;
  return ()

(* participant A *)
let tA_bad () : unit Lwt.t =
  let open Lwt in
  let sa = send b (fun x->x#msg) () sa in
  receive c sa >>= fun (`msg((), sa)) ->
  close sa;
  return ()

(* participant B *)
let tB : unit Lwt.t =
  let open Lwt in
  receive a sb >>= fun (`msg((), sb)) ->
  let sb = send c (fun x->x#msg) () sb in
  close sb;
  return ()

(* participant C *)
let tC : unit Lwt.t =
  let open Lwt in
  receive b sc >>= fun (`msg((), sc)) ->
  let sc = send a (fun x->x#msg) () sc in
  close sc;
  return ()

let () =
  Lwt_main.run (Lwt.join [tA; tB; tC])
