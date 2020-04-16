module M = struct
  module Base = Base
  module DynLin = DynLin
  module Comm = Comm
  module Shared = Shared
  module Util = Util
end

include M.Base
include M.Comm.Dyn
include M.Shared.Dyn

module Util = M.Util

exception InvalidEndpoint = DynLin.InvalidEndpoint
exception UnguardedLoop = Mergeable.UnguardedLoop
exception UnguardedLoopSeq = Seq.UnguardedLoopSeq
