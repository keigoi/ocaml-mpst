include Mpst.Internal.Base
include Combinators_lin
include Shared_lin

module Util = Mpst.Util

exception InvalidEndpoint = Mpst.InvalidEndpoint
exception UnguardedLoop = Mpst.UnguardedLoop
exception UnguardedLoopSeq = Mpst.UnguardedLoopSeq

module Internal = struct
  module Combinators = Combinators_lin
  module Shared = Shared_lin
end

