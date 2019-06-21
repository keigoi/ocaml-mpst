module Peripheral = Peripheral

module Global =
  Mpst.Global.Make
    (Mpst.Dyncheck)
    (Peripheral.AsyncMonad)
    (Peripheral.AsyncEvent)
    (Peripheral.AsyncSerial)
    (struct type 'a lin = 'a let mklin x = x let unlin x = x end)

module Local =
  Mpst.Local.Make
    (Mpst.Dyncheck)
    (Mpst.LinFlag)
    (Peripheral.AsyncMonad)
    (Peripheral.AsyncEvent)

module Default = struct
  include Mpst.Base
  include Global
  include Local
  include Mpst.Util.Make(Mpst.Dyncheck)
end

include Default
