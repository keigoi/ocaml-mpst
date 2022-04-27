module Context = State.Context

type _ t

val determinise : State.context -> 'a t -> 'a t
val to_string : State.context -> 'a t -> string
val force : State.context -> 'a t -> unit
val make : ('var, 't) Rows.constr -> 't State.t -> 'var t
val merge : State.context -> 'a t list -> 'a t list -> 'a t list
val match_item : 'var t -> int * 'var