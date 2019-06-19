module Base = Base
module S = S

module LinFlag : S.LIN_FLAG = LinFlag
module Seq : S.SEQ with type 'a mergeable = 'a Mergeable.t = Seq
module Mergeable : S.MERGEABLE = Mergeable

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
