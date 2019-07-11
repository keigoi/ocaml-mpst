module NanoMutexFlag : Mpst.S.FLAG =
struct
  type t         = Nano_mutex.t
  exception InvalidEndpoint

  let create ()  = Nano_mutex.create ()
  let try_lock_nano f = match Nano_mutex.try_lock f with Ok `Acquired -> true | Ok `Not_acquired | Error _ -> false
  let use f      =
    if not (try_lock_nano f) then raise InvalidEndpoint
  let try_use f  = try_lock_nano f
end
