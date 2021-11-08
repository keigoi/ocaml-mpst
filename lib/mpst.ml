include Types

module NoStaticLinearityChecking = struct
  type 'a lin = 'a

  let mklin x = x
end

include Types
include Global_combinators.Make (Dyn_lin.Check) (NoStaticLinearityChecking)
include Shared.Make (Dyn_lin.Check) (NoStaticLinearityChecking)

exception InvalidEndpoint = Mutex_flag.InvalidEndpoint
exception UnguardedLoop = Mergeable.UnguardedLoop
exception UnguardedLoopSeq = Seq.UnguardedLoopSeq

module S = S
module Types = Types
module Global_combinators = Global_combinators
module Env = Env
module Shared = Shared
module Util = Util
module Dyn_lin = Dyn_lin

module Internal = struct
  module Flag = Mutex_flag
  module Stream_opt = Stream_opt
end
