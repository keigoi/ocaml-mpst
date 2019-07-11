module M = struct
  module Peripheral = Peripheral
  module P = Peripheral
end

module Global =
  Mpst.Global.Make
    (Mpst.EP)
    (Mpst.Lin.NoCheck)
    (M.P.AsyncMonad)
    (M.P.AsyncEvent)
    (M.P.AsyncSerial)

module Local =
  Mpst.Local.Make
    (Mpst.EP)
    (M.P.AsyncMonad)
    (M.P.AsyncEvent)

module Default = struct
  include Mpst.Base
  include Global
  include Local
  include Mpst.Util.Make(Mpst.EP)
end

include M
include Default
