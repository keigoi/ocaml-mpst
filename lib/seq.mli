(**
 * The module for sequences of mergeables (endpoints).
 *)
module Make(EP:S.ENDPOINTS) : sig
  (** sequence type *)
  type _ t

  (** raised when one tries to extract a value from unguarded loop *)
  exception UnguardedLoopSeq

  (** lens get function *)
  val lens_get : ('a, _, 'aa, _) Base.lens -> 'aa t -> 'a EP.t
    
  (** lens put function *)
  val lens_put : ('a, 'b, 'aa, 'bb) Base.lens -> 'aa t -> 'b EP.t -> 'bb t

  (** merging of two sequences in a choice  *)
  val seq_merge : 'a t -> 'a t -> 'a t
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
  val int_of_lens : ('a, 'b, 'xs, 'ys) Base.lens -> int
  val effective_length : 'x t -> int
  val recvar : 'a t lazy_t -> 'a t
  val repeat : int -> (int -> 'a EP.t) -> ([`cons of 'a * 'tl] as 'tl) t
end
