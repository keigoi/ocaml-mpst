module Base = Base
module Common = Common
module S = S

module LinFlag : S.LIN_FLAG = LinFlag
module Mergeable = Mergeable
module Seq = Seq
module Util = Util
module Peripheral = Peripheral

module Global = struct
  module Make = Global.Make
  module Direct =
    Global.Make
      (Peripheral.Pure)(Peripheral.Event)(Peripheral.Serial)
      (struct type 'a lin = 'a let mklin x = x let unlin x = x end)
end

module Local = struct
  module Out = Out
  module Inp = Inp
  module Make = Local.Make
  module Direct = Local.Make(Peripheral.Pure)(Peripheral.Event)
end

module Default = struct
  include Base
  include Global.Direct
  include Local.Direct
  include Util
end

include Default
