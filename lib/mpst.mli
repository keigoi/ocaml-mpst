(** {1 Multiparty Session Types in OCaml} *)

include S.SHARED with type 'a lin := 'a
          (* and type ('v,'t) out = ('v,'t) Combinators.Make(Dyn_lin.Check)(NoStaticLinearityChecking).out
           * and type 'var inp = 'var Combinators.Make(Dyn_lin.Check)(NoStaticLinearityChecking).inp
           * and type close = Combinators.Make(Dyn_lin.Check)(NoStaticLinearityChecking).close
           * and type ('v,'t) scatter = ('v,'t) Combinators.Make(Dyn_lin.Check)(NoStaticLinearityChecking).scatter
           * and type 'var gather = 'var Combinators.Make(Dyn_lin.Check)(NoStaticLinearityChecking).gather
           * and type 't global = 't Combinators.Make(Dyn_lin.Check)(NoStaticLinearityChecking).global
           * and type 't tup = 't Combinators.Make(Dyn_lin.Check)(NoStaticLinearityChecking).tup
           * and type 't ty = 't Combinators.Make(Dyn_lin.Check)(NoStaticLinearityChecking).ty
           * and type 't shared = 't Shared.Make(Dyn_lin.Check)(NoStaticLinearityChecking).shared *)

exception InvalidEndpoint
exception UnguardedLoop
exception UnguardedLoopSeq

module S = S
module Types = Types
module Combinators = Combinators
module Shared = Shared
module Util = Util

