(** Monitor *)
type 'a t
type 'b wait = WaitMore | Return of 'b
val create : 'a -> 'a t
val signal : 'a t -> unit
val wait : 'a t -> ('a -> 'b wait) -> 'b
val lock : 'a t -> ('a -> 'b) -> 'b
val try_lock : 'a t -> ('a -> 'b) -> 'b -> 'b
val get : 'a t -> 'a
