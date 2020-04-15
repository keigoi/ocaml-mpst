module M = struct
  module Base = Base
  module DynLin = DynLin
  module Comm = Comm
  module Shared = Shared
end

include M.Base
include M.Comm.Dyn
include M.Shared.Dyn

module Util = Util

exception InvalidEndpoint = DynLin.InvalidEndpoint
exception UnguardedLoop = Mergeable.UnguardedLoop
exception UnguardedLoopSeq = Seq.UnguardedLoopSeq
