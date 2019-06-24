module M = struct
  module Peripheral = Peripheral
  module P = Peripheral
end

module Global =
  Mpst.Global.Make
    (Mpst.Dyncheck)
    (M.P.AsyncMonad)
    (M.P.AsyncEvent)
    (M.P.AsyncSerial)
    (struct type 'a lin = 'a let mklin x = x let unlin x = x end)

module Local =
  Mpst.Local.Make
    (Mpst.Dyncheck)
    (Mpst.LinFlag)
    (M.P.AsyncMonad)
    (M.P.AsyncEvent)

module Default = struct
  include Mpst.Base
  include Global
  include Local
  include Mpst.Util.Make(Mpst.Dyncheck)
end

include M
include Default
