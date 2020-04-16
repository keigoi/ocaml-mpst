module M = struct
  module Base = Mpst.M.Base
  module Comm = Comm_lin
  module Shared = Shared_lin
end

include M.Base
include M.Comm
include M.Shared

module Util = Mpst.M.Util

exception InvalidEndpoint = Mpst.InvalidEndpoint
exception UnguardedLoop = Mpst.UnguardedLoop
exception UnguardedLoopSeq = Mpst.UnguardedLoopSeq
