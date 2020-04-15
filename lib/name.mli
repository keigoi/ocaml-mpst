type 'a out
type 'a inp
type 'a scatter
type 'a gather

val create : ('a -> 'b) -> 'a out * 'b inp

val merge_out : 'a out -> 'a out -> 'a out
val merge_inp : 'a inp -> 'a inp -> 'a inp

val send : 'a out -> 'a -> unit Lwt.t
val receive : 'a inp -> 'a Lwt.t

val create_scatter : int -> (int -> 'a -> 'b) -> 'a scatter * 'b inp list
val merge_scatter : 'a scatter -> 'a scatter -> 'a scatter
val send_many : 'a scatter -> (int -> 'a) -> unit Lwt.t

val create_gather : int -> ('a list -> 'b) -> 'a out list * 'b gather
val merge_gather : 'a gather -> 'a gather -> 'a gather
val receive_many : 'a gather -> 'a Lwt.t

