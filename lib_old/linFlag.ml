exception InvalidEndpoint

module PosixMutexFlag : S.FLAG = struct
  type t         = Mutex.t
  let create ()  = Mutex.create ()
  let use f      = if not (Mutex.try_lock f) then raise InvalidEndpoint
end

module NoCheckFlag : S.FLAG = struct
  type t         = unit
  let create ()  = ()
  let use _      = ()
end
