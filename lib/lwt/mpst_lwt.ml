module Global =
  Mpst.Global.Make
    (Peripheral.Lwt)
    (Peripheral.LwtEvent)
    (Peripheral.LwtSerial)
    (struct type 'a lin = 'a let mklin x = x let unlin x = x end)

module Default = struct
  include Mpst.Base
  include Global
  include Global.Local
  include Mpst.Util
end

include Default
