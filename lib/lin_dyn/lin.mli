exception InvalidEndpoint

type +'a lin
(** Linear type constructor *)

type 's gen

val declare : 'a -> 'a lin gen
(** Declare a linear resource *)

val fresh : 'a gen -> 'a
(** Generate a fresh linear resource *)

val use : 'a lin -> 'a
(** Extract the value. Raises InvalidEndpoint if the endpoint is already
    consumed *)

val map_gen : ('a -> 'b) -> 'a gen -> 'b gen
(** Wrap a linear resource *)

val map_lin : ('a -> 'b) -> 'a lin -> 'b lin
(** Wrap a linear resource *)

val raw_lin : 'a lin -> 'a
val raw_gen : 'a gen -> 'a
val merge_gen : ('a -> 'b -> 'c) -> 'a gen -> 'b gen -> 'c gen

val merge_lin : ('a -> 'b -> 'c) -> 'a lin -> 'b lin -> 'c lin
(** Merge two linear values (having the same flag) *)

val lift_disj : ('lr, 'l, 'r) Rows.disj -> ('lr gen, 'l gen, 'r gen) Rows.disj
(** Lift given disjoint concatenation *)

val declare_unlimited : 'a -> 'a gen
