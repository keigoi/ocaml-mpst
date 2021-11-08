type t

val create : unit -> t
val use : t -> unit Concur_shims.IO.io

exception InvalidEndpoint
