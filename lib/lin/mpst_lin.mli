(** Mpst_lin *)

(** Mpst *)

(** {1 Communication Primitives} *)

include S.COMM_LIN

(** {1 Global Combinators} *)

include Mpst.S.GLOBAL_COMBINATORS
    with type 'a lin := 'a Linocaml.lin
    and type ('v,'t) out := ('v,'t) out
    and type 'var inp := 'var inp
    and type ('v,'t) out_many := ('v,'t) out_many
    and type 'var inp_many := 'var inp_many
    and type close := close

(** {1 Creating Channels} *)

include S.GEN_LIN
    with type 't global := 't global
    and type 't lin := 't Linocaml.lin
    and type 'a one := 'a one
    and type ('a,'b,'c,'d,'e,'f) role := ('a,'b,'c,'d,'e,'f) role
    and type close := close

(** {1 Creating Ports} *)

include S.PORTS_LIN
    with type 't global := 't global
    and type ('a,'b,'c,'d,'e,'f) role := ('a,'b,'c,'d,'e,'f) role
    and type 'a one := 'a one

(** {1 Exceptions } *)

exception InvalidEndpoint
exception UnguardedLoop
exception UnguardedLoopSeq

(** {1 Sub-modules } *)

module S = S
module Global_combinators_lin = Global_combinators_lin
module Shared = Shared_lin
module Types = Mpst.Types
module Util = Mpst.Util
