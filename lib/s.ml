module type Name = sig
  type 'a name
  type 'a endpoint

  val unify : 'a name -> 'a name -> unit
  val finalise : 'a name -> 'a endpoint
end