module Base = Base
module Common = Common
module S = S

module LinFlag : S.LIN_FLAG = LinFlag
module Peripheral = Peripheral
module Util = Util

module Dyncheck = Dyncheck_ep.Make(LinFlag)

module Global = struct
  module Make = Global.Make
  module Direct =
    Global.Make
      (Dyncheck)
      (Peripheral.Pure)
      (Peripheral.Event)
      (Peripheral.Serial)
      (struct type 'a lin = 'a let mklin x = x let unlin x = x end)
end

module Local = struct
  module Out = Out
  module Inp = Inp
  module Make = Local.Make
  module Direct =
    Local.Make
      (Dyncheck)
      (LinFlag)
      (Peripheral.Pure)
      (Peripheral.Event)
end

module Default = struct
  include Base
  include Global.Direct
  include Local.Direct
  include Util.Make(Dyncheck)
end

include Default
