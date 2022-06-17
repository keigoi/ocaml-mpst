type chan
type 'a endpoint

val make : unit -> chan
val new_endpoint : chan -> 'a endpoint
val unify : 'a endpoint -> 'a endpoint -> unit
val finalise : 'a endpoint -> 'a endpoint
val send : 'a endpoint -> 'a -> unit
val receive : 'a endpoint -> 'a
