(* create your own labels (see util.ml for other examples) *)
open Mpst_shmem.Global
open Mpst_shmem.Lin
open Mpst_shmem.Lin.LinMonad
open Mpst_shmem.Lin.LinMonad.Op

let a = {role=`A; lens=Fst}
let b = {role=`B; lens=Next Fst}
let c = {role=`C; lens=Next (Next Fst)}

let finish = one @@ one @@ one @@ nil
let ep = Fst

let f () =
  send `A (fun x->x#msg) () ep >>- fun%lin #ep ->
  send `A (fun x->x#msg) () ep >>- fun%lin #ep ->
  return ()
