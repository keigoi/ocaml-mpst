module type Type = sig
  type 'a value
end

module type S = sig
  type t
  type 'a key
  type 'a value

  val make : unit -> t
  val new_key : unit -> 'a key
  val add_binding : t -> 'a key -> 'a value -> unit
  val lookup : t -> 'a key -> 'a value option
  val union_keys : 'a key -> 'a key -> 'a key
  val union_keys_general : 'a key -> 'b key -> ('a key, 'b key) Either.t
end

module Make : functor (X : Type) -> S with type 'a value = 'a X.value
