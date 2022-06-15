(** Concurrent, buffered channel *)

type 'a t
val create : unit -> 'a t
val send : 'a t -> 'a -> unit
val receive : 'a t -> 'a
val peek : 'a t -> 'a
val clear : 'a t -> unit
val is_empty : 'a t -> bool
val length : 'a t -> int
