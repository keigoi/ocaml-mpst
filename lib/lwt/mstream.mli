type 'a out
type 'a inp

val create_with : wrap:('a -> 'b) -> 'a out * 'b inp
val create : unit -> 'a out * 'a inp

val merge_inp : 'a inp -> 'a inp -> 'a inp
val merge_out : 'a out -> 'a out -> 'a out

val wrap_inp : 'a inp -> ('a -> 'b) -> 'b inp

val send : 'a out -> 'a -> unit Lwt.t
val receive : 'a inp -> 'a Lwt.t
