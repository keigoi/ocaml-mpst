module M = struct
  module Base = Base
  module Common = Common
  module S = S
  module Inp = Inp
  module Out = Out
  module Table = Table

  module LinFlag : S.LIN_FLAG = LinFlag
  module Peripheral = Peripheral
  module P = Peripheral
  module Global_common = Global_common
  module Util = Util
  module NoLin : S.LIN with type 'a lin = 'a = struct
    type 'a lin = 'a
    let mklin x = x
    let unlin x = x
  end

  module Dyncheck = Dyncheck_ep.Make(LinFlag)

  module Global = struct
    module Make = Global.Make
    module Direct =
      Global.Make
        (Dyncheck)
        (Peripheral.Pure)
        (Peripheral.Event)
        (Peripheral.Serial)
        (NoLin)
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
end

module Default = struct
  include Base
  include M.Global.Direct
  include M.Local.Direct
  include M.Util.Make(M.Dyncheck)
end

include M
include Default
