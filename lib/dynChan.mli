type chan
type 'a name
type 'a endpoint

val make : unit -> chan
val new_name : chan -> 'a name
val unify : 'a name -> 'a name -> unit
val finalise : 'a name -> 'a endpoint
val send : 'a endpoint -> 'a -> unit
val receive : 'a endpoint -> 'a
