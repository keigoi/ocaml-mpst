include (Shared_lin : S.SHARED_LIN)

exception InvalidEndpoint = Mpst.InvalidEndpoint
exception UnguardedLoop = Mpst.UnguardedLoop
exception UnguardedLoopSeq = Mpst.UnguardedLoopSeq

module S = S
module Global_combinators_lin = Global_combinators_lin
module Shared = Shared_lin
module Util = Mpst.Util
