open Base

type _ t

type 'a local = 'a DynLin.gen Mergeable.t

(** raised when one tries to extract a value from unguarded loop *)
exception UnguardedLoopSeq

(** lens get function *)
val get : ('a one, 'b, 'xs, 'ys) idx -> 'xs t -> 'a local
val get_list : size:int -> ('a list, 'b, 'xs, 'ys) idx -> 'xs t -> 'a local list

(** lens put function *)
val put :
  ('a, 'b one, 'xs, 'ys) idx -> 'xs t -> 'b local -> 'ys t
val put_list :
  ('a, 'b list, 'xs, 'ys) idx -> 'xs t -> 'b local list -> 'ys t

(** merging of two sequences in a choice  *)
val seq_merge : 'x t -> 'x t -> 'x t

val force_recvar : 'x t lazy_t list -> 'x t lazy_t -> 'x t

(**
 * resolve_merge:
 * it tries to expand unguarded recursion variables which occurs right under the
 * fixpoint combinator. This enables a "fail-fast" policy to handle unguarded recursions --
 * it would raise an exception if there is an unguarded occurrence of a recursion variable.
 * This fuction is called during the initial construction phase of an
 * endpoint sequence (fix).
*)
val resolve_merge : 'x t -> 'x t

val force_all : 'x t -> unit
val effective_length : 'x t -> int

val recvar : 'a t lazy_t -> 'a t
val repeat : int -> (int -> 'a local) -> ([ `cons of 'a one * 'b ] as 'b) t
