type 'a t

val make : unit -> 'a t
val unify : 'a t -> 'a t -> unit
val finalise : 'a t -> 'a Event.channel
