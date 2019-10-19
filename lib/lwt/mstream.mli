open Mpst.Base

type 'a st
type 'a out
type 'a inp

val create : num:int -> 'a st
val create_one : unit -> 'a one out * 'a one inp

val wrap : 'a st -> ('a -> 'b) -> 'a one out * 'b one inp

val merge_inp : 'a inp -> 'a inp -> 'a inp
val merge_out : 'a out -> 'a out -> 'a out

val send : 'a one out -> 'a -> unit Lwt.t
val receive : 'a one inp -> 'a Lwt.t

(** scatter/gather *)
val wrap_scatter : 'a st -> ('a -> 'b) -> 'a list out * 'b one inp list
val wrap_gather : 'a st -> ('a list -> 'b) -> 'a one out list * 'b list inp

val send_many : 'a list out -> (int -> 'a) -> unit Lwt.t
val receive_many : 'a list inp -> 'a Lwt.t
