open State

exception UnguardedLoop

module type PowOp = sig
  type _ t

  val determinise : context -> 's t -> 's t
  val merge : context -> 's t -> 's t -> 's t
  val force : context -> 'a t -> unit
  val to_string : context -> 'a t -> string
  val make : 'obj State.t -> 'obj t
end

include PowOp

val make_deterministic : 'obj State.id -> 'obj State.t lazy_t -> 'obj t
val make_internal_choice : ('a, 'b, 'c) Rows.disj -> 'b t -> 'c t -> 'a t
val make_lazy : 'a t lazy_t -> 'a t
val make_merge : 'a t -> 'a t -> 'a t
val unit : unit t
val determinise_core : context -> 's t -> 's State.id * 's State.t lazy_t
val do_determinise : 's t -> 's
val ensure_determinised : 's t -> 's
