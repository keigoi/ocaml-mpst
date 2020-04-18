open Concur_shims
exception InvalidEndpoint
type t = Nano_mutex.t
let create ()  = Nano_mutex.create ()
let try_lock_nano f =
  match Nano_mutex.try_lock f with
  | Ok `Acquired -> true
  | Ok `Not_acquired | Error _ -> false
let use f      =
  if try_lock_nano f then
    IO.return_unit
  else
    raise InvalidEndpoint
