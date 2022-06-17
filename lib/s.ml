module type CHANNEL = sig
  type t
  type 'a endpoint

  val make : unit -> t
  val new_endpoint : t -> 'a endpoint
  val unify : 'a endpoint -> 'a endpoint -> unit
  val finalise : 'a endpoint -> 'a endpoint
  val send : 'a endpoint -> 'a -> unit
  val receive : 'a endpoint -> 'a
end
