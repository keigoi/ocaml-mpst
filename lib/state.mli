type 'a head = 'a Head.head = {
  head : 'a;
  determinise_list : Head.context -> 'a list -> 'a;
  force_traverse : Head.context -> 'a -> unit;
  to_string : Head.context -> 'a -> string;
}

type 't state_id = 't Head.state_id
type _ t

exception UnguardedLoop

val unit : unit t
val deterministic : 'obj state_id -> 'obj head lazy_t -> 'obj t
val merge : 'a t -> 'a t -> 'a t
val internal_choice : ('a, 'b, 'c) Rows.disj -> 'b t -> 'c t -> 'a t
val loop : 'a t lazy_t -> 'a t

val determinise_head_list :
  Head.context -> 'a state_id -> 'a head lazy_t list -> 'a head lazy_t

val try_cast_then_merge_heads :
  Head.context ->
  'a state_id ->
  ('b, 'a) Rows.constr ->
  ('b, 'c) Rows.constr ->
  'a head lazy_t ->
  'c head lazy_t ->
  'a head lazy_t option

val force_traverse : Head.context -> 'a t -> unit
val to_string : Head.context -> 'a t -> string

module Determinise : sig
  val determinise : Head.context -> 's t -> 's state_id * 's head lazy_t
end

val determinise : 's t -> 's
val ensure_determinised : 's t -> 's
