open Concur_shims

type 'a t

val create : unit -> 'a t
val send : 'a t -> 'a -> unit IO.io
val receive : 'a t -> 'a IO.io
val receive_wrap : f:('a -> 'b option) -> 'a t -> 'b IO.io
