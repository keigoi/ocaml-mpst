type 'a head = 'a StateHash.head = {
  head : 'a;
  determinise_list : StateHash.t -> 'a list -> 'a;
  force_determinised : StateHash.t -> 'a -> unit;
  to_string : StateHash.t -> 'a -> string;
}

type 't state_id = 't StateHash.state_id

type _ t =
  | Deterministic : 'obj state_id * 'obj head lazy_t -> 'obj t
  | Epsilon : 'a t list -> 'a t
  | InternalChoice :
      'lr state_id * ('lr, 'l, 'r) Rows.disj * 'l t * 'r t
      -> 'lr t
  | Loop : 'a t lazy_t -> 'a t

exception UnguardedLoop of string

val unit : unit t
val merge : 'a t -> 'a t -> 'a t
val internal_choice : ('a, 'b, 'c) Rows.disj -> 'b t -> 'c t -> 'a t
val loop : 'a t lazy_t -> 'a t

val determinise_head_list :
  StateHash.t -> 'a state_id -> 'a head lazy_t list -> 'a StateHash.value

val try_cast_then_merge_heads :
  StateHash.t ->
  'a state_id ->
  ('b, 'a) Rows.constr ->
  ('b, 'c) Rows.constr ->
  'a head lazy_t ->
  'c head lazy_t ->
  'a StateHash.value option

val force_determinised : StateHash.t -> 'a t -> unit
val to_string : StateHash.t -> 'a t -> string

module Determinise : sig
  val determinise : StateHash.t -> 's t -> 's state_id * 's head lazy_t
end

val determinise : 's t -> 's