(** Mpst_lin *)

(** {1 Multiparty Session Types in OCaml} *)

include S.GLOBAL_COMBINATORS_LIN

(** {1 Exceptions } *)

exception InvalidEndpoint
exception UnguardedLoop
exception UnguardedLoopSeq

(** {1 Sub-modules } *)

module S = S
module Combinators = Combinators_lin
module Shared = Shared_lin
module Util = Mpst.Util
