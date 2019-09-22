type 'a t
val create : unit -> 'a t
val send : 'a t -> 'a -> unit Lwt.t
val receive : 'a t -> 'a Lwt.t
