type 'a st
type 'a out
type 'a inp

val create : ('a -> 'b) -> 'a out * 'b inp

val merge_inp : 'a inp -> 'a inp -> 'a inp
val merge_out : 'a out -> 'a out -> 'a out

val send : 'a out -> 'a -> unit Lwt.t
val receive : 'a inp -> 'a Lwt.t

