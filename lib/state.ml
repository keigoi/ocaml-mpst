module type Op0 = sig
  type a
  type context

  val determinise : context -> a -> a
  val merge : context -> a -> a -> a
  val force : context -> a -> unit
  val to_string : context -> a -> string
end

module rec Context : sig
  type t

  module type Op1 = Op0 with type context := t

  type 'a state = { st : 'a; st_ops : (module Op1 with type a = 'a) }
  type 'a value = 'a state lazy_t

  include ContextF.S with type t := t and type 'a value := 'a Context.value
end = struct
  module Ctx = ContextF.Make (Context)
  include Ctx

  module type Op1 = Op0 with type context := t

  type 'a state = { st : 'a; st_ops : (module Op1 with type a = 'a) }
  type 'a value = 'a state lazy_t
end

module type StateOp = Context.Op1

type context = Context.t
type 't id = 't Context.key
type 'a op = (module StateOp with type a = 'a)
type 'a t = 'a Context.state = { st : 'a; st_ops : 'a op }

let map_ops (type x y) (f : x -> y) (g : y -> x) (name : string -> string)
    (module D : StateOp with type a = x) : (module StateOp with type a = y) =
  let module M = struct
    type nonrec a = y

    let determinise ctx s = f @@ D.determinise ctx @@ g s

    let merge ctx s1 s2 =
      let inp1 = g s1 and inp2 = g s2 in
      f @@ D.merge ctx inp1 inp2

    let force ctx s = D.force ctx @@ g s
    let to_string ctx s = name @@ D.to_string ctx (g s)
  end in
  (module M)

let map (type x y) (f : x -> y) (g : y -> x) (name : string -> string)
    (s : x op) =
  map_ops f g name s

open Rows

let obj_op meth =
  map meth.make_obj meth.call_obj (fun s -> meth.method_name ^ s)

module Unit : StateOp with type a = unit = struct
  type a = unit

  let determinise _ _ = ()
  let merge _ _ _ = ()
  let force _ _ = ()
  let to_string _ _ = "end"
end
