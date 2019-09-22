module M = struct
  module Peripheral = Peripheral
  module P = Peripheral
  module Lwt_stream_opt = Lwt_stream_opt
end

module Global =
  Mpst.Global.Make
    (Mpst.EP)
    (Mpst.Lin.NoCheck)
    (M.Peripheral.Lwt)
    (M.Peripheral.LwtEvent)
    (M.Peripheral.LwtSerial)

module Local =
  Mpst.Local.Make
    (Mpst.EP)
    (M.Peripheral.Lwt)
    (M.Peripheral.LwtEvent)

module Default = struct
  include Mpst.Base
  include Global
  include Local
  include Mpst.Util.Make(Mpst.EP)
end

include M
include Default
