(** Concurrent, buffered channel *)

type 'a t
val create : unit -> 'a t
val send : 'a t -> 'a -> unit
val send_all : 'a t -> 'a list -> unit
val receive : 'a t -> 'a
val receive_all : 'a t -> ('b -> 'a -> 'b) -> 'b -> 'b
val receive_all_ : 'a t -> ('a -> unit) -> unit
val peek : 'a t -> 'a
val clear : 'a t -> unit
val is_empty : 'a t -> bool
val length : 'a t -> int
