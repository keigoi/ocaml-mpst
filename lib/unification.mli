type 'a t

val make : 'a -> 'a t
val get : 'a t -> 'a
val unify : 'a t -> 'a t -> unit
val path_compression : 'a t -> 'a t
