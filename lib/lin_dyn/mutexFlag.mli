type t

val create : unit -> t
val use : t -> unit

exception InvalidEndpoint
