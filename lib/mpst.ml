include Base
include Combinators.Dyn
include Shared.Dyn

module Util = Util

exception InvalidEndpoint = Mutex_flag.InvalidEndpoint
exception UnguardedLoop = Mergeable.UnguardedLoop
exception UnguardedLoopSeq = Seq.UnguardedLoopSeq

module Internal = struct
  module Combinators = Combinators
  module Shared = Shared
  module Base = Base
  module Flag = Mutex_flag
  module Dyn_lin = Dyn_lin
  module Stream_opt = Stream_opt
end
