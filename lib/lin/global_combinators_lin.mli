
module Lin : sig
  type +'a lin
  val mklin : 'a -> 'a lin
  val unlin_ : 'a lin -> 'a
end

include S.COMM_LIN

(** {1 Global Combinators} *)

include Mpst.S.GLOBAL_COMBINATORS
    with type 't global = 't Mpst.Global_combinators.Make(Mpst.Dyn_lin.NoCheck)(Lin).global
    and type 'a lin := 'a Linocaml.lin
    and type ('v,'t) out := ('v,'t) out
    and type 'var inp := 'var inp
    and type ('v,'t) out_many := ('v,'t) out_many
    and type 'var inp_many := 'var inp_many
    and type close := close

(** {1 Creating Channels} *)

include S.GEN_LIN
    with type 't global := 't global
    and type 't lin := 't Linocaml.lin
    and type close := close
