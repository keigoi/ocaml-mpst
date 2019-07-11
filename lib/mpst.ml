module M = struct
  module Base = Base
  module Common = Common
  module S = S
  module Inp = Inp
  module Out = Out
  module Lin = Lin
  module Table = Table
  module Endpoints = Endpoints

  module LinFlag = LinFlag
  module Peripheral = Peripheral
  module P = Peripheral
  module Global_common = Global_common
  module Util = Util
  (* module NoLin : S.LIN with type 'a lin = 'a = struct
   *   type 'a lin = 'a
   *   let mklin x = x
   *   let unlin x = x
   * end *)

  (* module Dyncheck_ep = Dyncheck_ep
   * module Nocheck = Nocheck
   * module Dyncheck = Dyncheck_ep.Make(LinFlag) *)

  module EP = Endpoints.Make(Lin.DynCheck)

  module Global = struct
    module Make = Global.Make
    module Direct =
      Global.Make
        (EP)
        (Lin.NoCheck)
        (Peripheral.Pure)
        (Peripheral.Event)
        (Peripheral.Serial)
  end

  module Local = struct
    module Out = Out
    module Inp = Inp
    module Make = Local.Make
    module Direct =
      Local.Make
        (EP)
        (Peripheral.Pure)
        (Peripheral.Event)
  end
end

module Default = struct
  include Base
  include M.Global.Direct
  include M.Local.Direct
  include M.Util.Make(M.EP)
end

include M
include Default
