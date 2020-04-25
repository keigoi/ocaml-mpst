open Types
    
type 'a t
(** A mergeable *)

exception UnguardedLoop

val make : value:'a -> mergefun:('a -> 'a -> 'a) -> ?cont:'x t -> unit -> 'a t
(** Make a mergeable value, with a merging function and an optional continuation *)

val merge : 'a t -> 'a t -> 'a t
(** Merge two mergeables *)

val make_recvar : 'a t lazy_t -> 'a t
(** Declare a delayed mergeable, which is used in a recursion variable (fix (fun t -> ...)).  *)

val make_disj : ('lr,'l,'r) disj -> 'l t -> 'r t -> 'lr t
(** Concatenate two disjoint mergeables into one using a `disj' *)

val resolve : 'a t -> 'a
(** Extract the value from a mergeable *)
