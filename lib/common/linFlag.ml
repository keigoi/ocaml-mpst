exception InvalidEndpoint
type t         = Mutex.t
let create ()  = Mutex.create ()
let use f      =
  if not (Mutex.try_lock f) then raise InvalidEndpoint
let try_use f  = Mutex.try_lock f
