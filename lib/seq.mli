(**
 * The module for sequences of mergeables (endpoints).
 *)
module Make(EP:S.ENDPOINT) : sig
type 'a mergeable = 'a Mergeable.Make(EP).t
(** sequence type *)
type _ t = (* can we hide these constructors? *)
  (** cons *)
  | SeqCons : 'hd mergeable * 'tl t -> [ `cons of 'hd * 'tl ] t
  (** repetition -- for closed endpoints *)
  | SeqRepeat : int * (int -> 'a mergeable) -> ([ `cons of 'a * 'b ] as 'b) t
  (** recursion variable(s) *)
  | SeqRecVars : 'a seqvar list -> 'a t
  (** unguarded loop; we have it in the last part of a recursion *)
  | SeqBottom : 'x t
and 'a seqvar = 'a t lazy_t

(** lenses in constructor form *)
type (_, _, _, _) lens =
    Zero : ('hd0, 'hd1, [ `cons of 'hd0 * 'tl ] t, [ `cons of 'hd1 * 'tl ] t) lens
  | Succ :
      ('a, 'b, 'tl0 t, 'tl1 t) lens
      -> ('a, 'b, [ `cons of 'hd * 'tl0 ] t, [ `cons of 'hd * 'tl1 ] t) lens

(** raised when one would try to extract a value from unguarded loop *)
exception UnguardedLoopSeq

(** lens get function *)
val get : ('a, 'b, 'xs, 'ys) lens -> 'xs -> 'a mergeable
(** lens put function *)
val put : ('a, 'b, 'xs, 'ys) lens -> 'xs -> 'b mergeable -> 'ys

(** lens get function *)
val int_of_lens : ('a, 'b, 'xs, 'ys) lens -> int

(** merging of two sequences in a choice  *)
val seq_merge : 'x t -> 'x t -> 'x t

(**
 * partial_force:
 * it tries to expand unguarded recursion variables which occurs right under the
 * fixpoint combinator. This enables a "fail-fast" policy to handle unguarded recursions --
 * it would raise an exception if there is an unguarded occurrence of a recursion variable.
 * This fuction is called during the initial construction phase of an
 * endpoint sequence (fix).
 *)
val partial_force : 'x t lazy_t list -> 'x t -> 'x t

val effective_length : 'x t -> int
end
