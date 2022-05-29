type context
type _ id

module type StateOp = sig
  type a

  val determinise : context -> a -> a
  val merge : context -> a -> a -> a
  val force : context -> a -> unit
  val to_string : context -> a -> string
end

type 'a t = { st : 'a; st_ops : (module StateOp with type a = 'a) }

module Unit : StateOp with type a = unit

module Context :
  ContextF.S
    with type 'a key = 'a id
     and type t := context
     and type 'a value := 'a t lazy_t

val map_ops :
  ('x -> 'y) ->
  ('y -> 'x) ->
  (string -> string) ->
  (module StateOp with type a = 'x) ->
  (module StateOp with type a = 'y)

val map : ('a -> 'b) -> ('b -> 'a) -> (string -> string) -> 'a t -> 'b t
val map_method : ('a, 'b) Rows.method_ -> 'b t -> 'a t
