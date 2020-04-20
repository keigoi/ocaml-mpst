open Concur_shims
exception InvalidEndpoint
let (let*) = IO.bind

type t         = Mutex.t
let create  = Mutex.create
let use f      =
  let* b = Mutex.try_lock f in
  if b then
    IO.return_unit
  else
    raise InvalidEndpoint
