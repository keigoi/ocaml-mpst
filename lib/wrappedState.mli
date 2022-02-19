type 'var t

val make : ('a, 'b) Types.constr -> unit Name.t -> 'b State.t -> 'a t
val merge : 'a t -> 'a t -> 'a t
val determinise : cache:StateHash.dict -> 'a t -> unit
val make_event_from_determinised_ : 'a t -> 'a Event.event
