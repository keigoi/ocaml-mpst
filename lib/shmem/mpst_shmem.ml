include Mpst_base

module Make(F:S.FLAG) = struct
  module Session = Session.Make(F)
  module Global = Global.Make(F)
  module Util = Util.Make(F)
end

include Make(Flags.NanoMutexFlag)
module Lin = LinSession.Make(Session)

module NoDynCheck = struct
  include Make(Flags.NoFlag)
  module Lin = LinSession.Make(Session)
end
