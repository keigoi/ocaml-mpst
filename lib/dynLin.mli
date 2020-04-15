open Concur_shims
open Base

exception InvalidEndpoint

type +'a lin
(** Linear type constructor *)

type 's gen

val declare : 'a -> 'a lin gen
(** Declare a linear resource *)

val fresh : 'a gen -> 'a
(** Generate a fresh linear resource  *)

val use : 'a lin -> 'a IO.io
(** Extract the value. Raises InvalidEndpoint if the endpoint is already consumed *)

val wrap : ('a -> 'b) -> 'a gen -> 'b gen
(** Wrap a linear resource *)

val merge : ('b -> 'b -> 'b) -> ('a, 'b lin) method_ -> 'a gen -> 'a gen -> 'a gen
(** Merge two linear values (having the same flag) *)

val lift_disj : ('lr,'l,'r) disj -> ('lr gen, 'l gen, 'r gen) disj
(** Lift given disjoint concatenation *)

val declare_unlimited : 'a -> 'a gen
