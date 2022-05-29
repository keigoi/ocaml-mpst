type context

module type StateOp = sig
  type a

  val determinise : context -> a -> a
  val merge : context -> a -> a -> a
  val force : context -> a -> unit
  val to_string : context -> a -> string
end

module Unit : StateOp with type a = unit

type 'a state = { st : 'a; st_ops : (module StateOp with type a = 'a) }
type _ state_id

val map_ops :
  ('x -> 'y) ->
  ('y -> 'x) ->
  (string -> string) ->
  (module StateOp with type a = 'x) ->
  (module StateOp with type a = 'y)

val map : ('a -> 'b) -> ('b -> 'a) -> (string -> string) -> 'a state -> 'b state
val map_method : ('a, 'b) Rows.method_ -> 'b state -> 'a state

module Context :
  ContextF.S
    with type 'a key = 'a state_id
     and type t := context
     and type 'a value := 'a state lazy_t