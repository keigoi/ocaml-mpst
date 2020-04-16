module M = struct
  module Base = Base
  module DynLin = DynLin
  module Comm = Comm
  module Shared = Shared
  module Util = Util
  module Flag = Mutex_flag
  module Stream_opt = Stream_opt
end

include M.Base
include M.Comm.Dyn
include M.Shared.Dyn

module Util = M.Util

exception InvalidEndpoint = DynLin.InvalidEndpoint
exception UnguardedLoop = Mergeable.UnguardedLoop
exception UnguardedLoopSeq = Seq.UnguardedLoopSeq
