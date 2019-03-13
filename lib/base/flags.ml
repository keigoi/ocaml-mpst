module NoFlag : S.FLAG =
struct
  type t         = unit
  let create ()  = ()
  let use _      = ()
  let try_use f  = true
end

module NanoMutexFlag : S.FLAG =
struct
  type t         = Core.Nano_mutex.t
  let create ()  = Core.Nano_mutex.create ()
  let try_lock_nano f = match Core.Nano_mutex.try_lock f with Ok `Acquired -> true | Ok `Not_acquired | Error _ -> false
  let use f      =
    if not (try_lock_nano f) then raise Base.InvalidEndpoint
  let try_use f  = try_lock_nano f
end
