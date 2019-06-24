module M = struct
  module Peripheral = Peripheral
  module P = Peripheral
end

module Global =
  Mpst.Global.Make
    (Mpst.Dyncheck)
    (M.Peripheral.Lwt)
    (M.Peripheral.LwtEvent)
    (M.Peripheral.LwtSerial)
    (Mpst.NoLin)

module Local =
  Mpst.Local.Make
    (Mpst.Dyncheck)
    (Mpst.LinFlag)
    (M.Peripheral.Lwt)
    (M.Peripheral.LwtEvent)

module Default = struct
  include Mpst.Base
  include Global
  include Local
  include Mpst.Util.Make(Mpst.Dyncheck)
end

include M
include Default
