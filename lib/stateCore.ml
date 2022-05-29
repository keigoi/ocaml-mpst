open Rows

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
type 't state_id = 't Context.key

type 'a state = 'a Context.state = {
  st : 'a;
  st_ops : (module StateOp with type a = 'a);
}
