module Global =
  Mpst.Global.Make
    (Peripheral.AsyncMonad)
    (Peripheral.AsyncEvent)
    (Peripheral.AsyncSerial)
    (struct type 'a lin = 'a let mklin x = x let unlin x = x end)

module Local =
  Mpst.Local.Make
    (Peripheral.AsyncMonad)
    (Peripheral.AsyncEvent)

module Default = struct
  include Mpst.Base
  include Global
  include Local
  include Mpst.Util
end

include Default
