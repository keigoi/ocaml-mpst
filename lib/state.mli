exception UnguardedLoop

type _ t
type _ state_id

val determinise : 's t -> 's
val to_string : 'a t -> string
val unit : unit t
val merge : 'a t -> 'a t -> 'a t
val make_internal_choice : ('a, 'b, 'c) Rows.disj -> 'b t -> 'c t -> 'a t
val make_loop : 'a t lazy_t -> 'a t
val ensure_determinised : 's t -> 's

type context

module type DetState = sig
  type a

  val determinise : context -> a list -> a
  val force : context -> a -> unit
  val to_string : context -> a -> string
end

type 'a det_state = {
  det_state : 'a;
  det_ops : (module DetState with type a = 'a);
}

val make_deterministic : 'obj state_id -> 'obj det_state lazy_t -> 'obj t

module Context :
  ContextF.S
    with type 'a key = 'a state_id
     and type t := context
     and type 'a value := 'a det_state lazy_t

val determinise_core : context -> 's t -> 's state_id * 's det_state lazy_t
val to_string_core : context -> 'a t -> string
val force_core : context -> 'a t -> unit

val merge_det :
  context ->
  'a state_id ->
  'a det_state lazy_t ->
  'a det_state lazy_t ->
  'a det_state lazy_t

val try_merge_det :
  context ->
  'a state_id ->
  ('b, 'a) Rows.constr ->
  ('b, 'c) Rows.constr ->
  'a det_state lazy_t ->
  'c det_state lazy_t ->
  'a det_state lazy_t option
