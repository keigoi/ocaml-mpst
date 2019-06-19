module Base = Base

module LinFlag : S.LIN_FLAG = LinFlag
module Mergeable : S.MERGEABLE = Mergeable
module Seq : S.SEQ = Seq
                    
module Local = Local
module Global = Global
module Util = Util

module Common = struct
  include Base
  include Global
  include Local
  include Util
end

include Common
