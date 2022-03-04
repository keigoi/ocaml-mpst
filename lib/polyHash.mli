module type Type = sig
  type 'a head
end

module type S = sig
  type t
  type 'a state_id

  type 'a head

  val make : unit -> t
  val add_binding : t -> 'a state_id -> 'a head lazy_t -> unit
  val lookup : t -> 'a state_id -> 'a head lazy_t option
  val make_key : unit -> 'a state_id
  val union_keys : 'a state_id -> 'a state_id -> 'a state_id

  val general_union_keys :
    'a state_id -> 'b state_id -> ('a state_id, 'b state_id) Either.t
end

module Make : functor (X : Type) -> S with type 'a head = 'a X.head
