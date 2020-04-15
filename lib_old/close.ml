open Base
module Make(M:S.MONAD) : sig
  type close
  val make_close : (unit -> unit M.t) -> close
  val close : close -> unit M.t
end = struct
  type close = Close of (unit -> unit M.t)
  let make_close f = Close f
  let close (Close f) = f ()
end


