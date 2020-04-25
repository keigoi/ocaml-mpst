(** Mpst *)

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

(** {1 Exceptions } *)

exception InvalidEndpoint
exception UnguardedLoop
exception UnguardedLoopSeq

(** {1 Sub-modules } *)

module S = S
module Types = Types
module Global_combinators = Global_combinators
module Shared = Shared
module Util = Util
module Dyn_lin = Dyn_lin

module Internal : sig
  module Flag : module type of Mutex_flag
  module Stream_opt : module type of Stream_opt
end