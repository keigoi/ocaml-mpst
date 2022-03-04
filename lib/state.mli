type context

type 'a det = {
  body : 'a;
  determinise_list : context -> 'a list -> 'a;
  force_traverse : context -> 'a -> unit;
  to_string : context -> 'a -> string;
}

module StateHash :
  PolyHash.S with type t := context and type 'a value := 'a det lazy_t

type _ t
type 'a state_id = 'a StateHash.key

exception UnguardedLoop

val unit : unit t
val deterministic : 'obj state_id -> 'obj det lazy_t -> 'obj t
val merge : 'a t -> 'a t -> 'a t
val internal_choice : ('a, 'b, 'c) Rows.disj -> 'b t -> 'c t -> 'a t
val loop : 'a t lazy_t -> 'a t

val determinise_list :
  context -> 'a state_id -> 'a det lazy_t list -> 'a det lazy_t

val try_cast_and_merge_determinise :
  context ->
  'a state_id ->
  ('b, 'a) Rows.constr ->
  ('b, 'c) Rows.constr ->
  'a det lazy_t ->
  'c det lazy_t ->
  'a det lazy_t option

val determinise_core : context -> 's t -> 's state_id * 's det lazy_t
val determinise : 's t -> 's
val ensure_determinised : 's t -> 's
val force_traverse : context -> 'a t -> unit
val to_string : context -> 'a t -> string
