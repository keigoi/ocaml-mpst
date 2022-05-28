exception UnguardedLoop

type _ t
type _ state_id

val determinise : 's t -> 's
val to_string : 'a t -> string
val unit : unit t
val merge : 'a t -> 'a t -> 'a t
val make_internal_choice : ('a, 'b, 'c) Rows.disj -> 'b t -> 'c t -> 'a t
val make_lazy : 'a t lazy_t -> 'a t

type context

module type StateOp = sig
  type a

  val determinise : context -> a -> a
  val merge : context -> a -> a -> a
  val force : context -> a -> unit
  val to_string : context -> a -> string
end

module Unit : StateOp with type a = unit

type 'a state = {
  st : 'a;
  st_ops : (module StateOp with type a = 'a);
}

val make_deterministic : 'obj state_id -> 'obj state lazy_t -> 'obj t

module Context :
  ContextF.S
    with type 'a key = 'a state_id
     and type t := context
     and type 'a value := 'a state lazy_t

val determinise_core : context -> 's t -> 's t
val determinise_core_ : context -> 's t -> 's state_id * 's state lazy_t
val merge_core : context -> 's t -> 's t -> 's t
val force_core : context -> 'a t -> unit
val to_string_core : context -> 'a t -> string
val ensure_determinised : 's t -> 's

val det_wrap_obj :
  ('obj, 'b) Rows.method_ ->
  (module StateOp with type a = 'b) ->
  (module StateOp with type a = 'obj)
