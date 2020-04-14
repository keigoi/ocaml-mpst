type +'a one = One of 'a
type 'a elem
type _ t

(** raised when one tries to extract a value from unguarded loop *)
exception UnguardedLoopSeq

(** lens get function *)
val get : ('a one, 'b, 'xs, 'ys) Base.idx -> 'xs t -> 'a Mergeable.t
val get_list : size:int -> ('a list, 'b, 'xs, 'ys) Base.idx -> 'xs t -> 'a Mergeable.t list

(** lens put function *)
val put :
  ('a one, 'b one, 'xs, 'ys) Base.idx -> 'xs t -> 'b Mergeable.t -> 'ys t
val put_list :
  ('a list, 'b list, 'xs, 'ys) Base.idx -> 'xs t -> 'b Mergeable.t list -> 'ys t

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
val repeat : int -> (int -> 'a elem) -> ([ `cons of 'a * 'b ] as 'b) t
