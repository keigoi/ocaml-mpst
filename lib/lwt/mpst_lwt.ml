module Peripheral = Peripheral

module Global =
  Mpst.Global.Make
    (Peripheral.Lwt)
    (Peripheral.LwtEvent)
    (Peripheral.LwtSerial)
    (struct type 'a lin = 'a let mklin x = x let unlin x = x end)

module Local =
  Mpst.Local.Make
    (Peripheral.Lwt)
    (Peripheral.LwtEvent)

module Default = struct
  include Mpst.Base
  include Global
  include Local
  include Mpst.Util
end

include Default
