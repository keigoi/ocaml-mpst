module Base = Base
module Common = Common
module S = S

module LinFlag : S.LIN_FLAG = LinFlag
module Seq = Seq
module Mergeable = Mergeable
module Util = Util

module Global = struct
  module Make
         (M:S.MONAD)
         (E:S.EVENT with type 'a monad = 'a M.t)
         (C:S.SERIAL with type 'a monad = 'a M.t)
         (Lin:S.LIN)
       = Global.Make(M)(E)(C)(Lin)

  module Direct =
    Global.Make
      (Peripheral.Pure)(Peripheral.Event)(Peripheral.Serial)
      (struct type 'a lin = 'a let mklin x = x let unlin x = x end)

  module Local = struct
    module Make
             (M:S.MONAD)
             (E:S.EVENT with type 'a monad = 'a M.t)
      = Local.Make(M)(E)

    module Direct = Direct.Local
  end
end

module Default = struct
  include Base
  include Global.Direct
  include Global.Local.Direct
  include Util
end

include Default
