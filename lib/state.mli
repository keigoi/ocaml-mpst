type context
type _ id

module type Op = sig
  type a

  val determinise : context -> a -> a
  val merge : context -> a -> a -> a
  val force : context -> a -> unit
  val to_string : context -> a -> string
end

type 'a op = (module Op with type a = 'a)

val obj_op : ('a, 'b) Rows.method_ -> 'b op -> 'a op

type 'a t = { st : 'a; st_op : 'a op }

val determinise_list : context -> 'a id -> 'a t lazy_t list -> 'a t lazy_t

module Unit : Op with type a = unit

module Context :
  ContextF.S
    with type 'a key = 'a id
     and type t := context
     and type 'a value := 'a t lazy_t
