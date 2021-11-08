(** Mpst *)

(* {0 MPST in OCaml} *)

(** {1 Communication Primitives} *)

include S.COMM

(** {1 Auxiliary Types} *)

include module type of Types

(** {1 Global Combinators} *)

include
  S.GLOBAL_COMBINATORS
    with type 'a lin := 'a
     and type ('v, 't) out := ('v, 't) out
     and type 'var inp := 'var inp
     and type ('v, 't) out_many := ('v, 't) out_many
     and type 'var inp_many := 'var inp_many
     and type close := close

(** {1 Creating Channels} *)

include
  S.GEN
    with type 't global := 't global
     and type 't ty := 't ty
     and type 't lin := 't
     and type close := close

(** {1 Creating Ports} *)

include S.PORTS with type 't global := 't global

(** {1 Exceptions} *)

exception UnguardedLoop
exception UnguardedLoopSeq

(** {1 Sub-modules} *)

module S = S
module Types = Types
module Global_combinators = Global_combinators
module Env = Env
module Shared = Shared
module Util = Util
module Dyn_lin = Dyn_lin

module Internal : sig
  module Flag : module type of Mutex_flag
  module Stream_opt : module type of Stream_opt
end
