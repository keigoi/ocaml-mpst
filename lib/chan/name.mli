type 'a name
type 'a endpoint = 'a Event.channel

val new_name : unit -> 'a name
val unify : 'a name -> 'a name -> unit
val finalise : 'a name -> 'a endpoint
