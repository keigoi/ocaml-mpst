exception InvalidEndpoint

type t = Mutex.t

let create = Mutex.create

let use f =
  let b = Mutex.try_lock f in
  if not b then raise InvalidEndpoint
