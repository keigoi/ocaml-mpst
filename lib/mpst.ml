module NoStaticLinearityChecking = struct
  type 'a lin = 'a
  let mklin x = x
end

include (Shared.Make(Dyn_lin.Check)(NoStaticLinearityChecking) :
           S.SHARED
         with type 'a lin := 'a
          and type ('v,'t) out = ('v,'t) Global_combinators.Make(Dyn_lin.Check)(NoStaticLinearityChecking).out
          and type 'var inp = 'var Global_combinators.Make(Dyn_lin.Check)(NoStaticLinearityChecking).inp
          and type close = Global_combinators.Make(Dyn_lin.Check)(NoStaticLinearityChecking).close
          and type ('v,'t) scatter = ('v,'t) Global_combinators.Make(Dyn_lin.Check)(NoStaticLinearityChecking).scatter
          and type 'var gather = 'var Global_combinators.Make(Dyn_lin.Check)(NoStaticLinearityChecking).gather
          and type 't global = 't Global_combinators.Make(Dyn_lin.Check)(NoStaticLinearityChecking).global
          and type 't tup = 't Global_combinators.Make(Dyn_lin.Check)(NoStaticLinearityChecking).tup
          and type 't ty = 't Global_combinators.Make(Dyn_lin.Check)(NoStaticLinearityChecking).ty
          and type 't shared = 't Shared.Make(Dyn_lin.Check)(NoStaticLinearityChecking).shared
        )

exception InvalidEndpoint = Mutex_flag.InvalidEndpoint
exception UnguardedLoop = Mergeable.UnguardedLoop
exception UnguardedLoopSeq = Seq.UnguardedLoopSeq

module S = S
module Types = Types
module Combinators = Combinators
module Shared = Shared
module Util = Util
module Dyn_lin = Dyn_lin
