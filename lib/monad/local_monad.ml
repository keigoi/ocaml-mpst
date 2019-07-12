type 'a lin = 'a Linocaml.lin
type 'a data = 'a Linocaml.data

module Make
         (M:Mpst.S.MONAD)
         (EV:Mpst.S.EVENT with type 'a monad = 'a M.t)
         (L:Linocaml.S.S with type 'a IO.io = 'a EV.monad) : sig

  type ('p,'q,'a) monad = ('p,'q,'a) L.monad

  type 't out = 't Mpst.Local.Make(Linocaml_lin.EP)(M)(EV).out
  type 't inp = 't Mpst.Local.Make(Linocaml_lin.EP)(M)(EV).inp
  type 't lin_ = 't Linocaml_lin.EP.lin

  val ( @* ) :
    ('a,'b,'q,'r) Linocaml.lens
    -> ('c,unit,'p,'q) Linocaml.lens
    -> ('a * 'c,'b,'p,'r) Linocaml.lens

  val send :
    ((< .. > as 'ep) -> ('t data Mpst.one * 'u) out lin_)
    -> 't
    -> ('ep lin, unit, 'u lin) monad

  val deleg_send :
    ((< .. > as 'ep) -> ('t lin Mpst.one * 'u) out lin_)
    -> ('ep lin * 't lin, unit, 'u lin) monad

  val receive :
    ((< .. > as 'ep) -> 't inp lin_)
    -> ('ep lin,unit,'t lin) monad

  val close :
    (Mpst.close lin,unit,unit data) monad

end = struct

  type ('p,'q,'a) monad = ('p,'q,'a) L.monad

  let[@inline] ( @* ) l1 l2 =
    let open Linocaml in
    let[@inline] get p =
      let c,q = lens_get l2 p, lens_put l2 p () in
      let a = lens_get l1 q in
      a,c
    and[@inline] put p b =
      let q = lens_put l2 p () in
      let r = lens_put l1 q b in
      r
    in
    Other(get,put)

  module Local = Mpst.Local.Make(Linocaml_lin.EP)(M)(EV)

  type 't out = 't Local.out
  type 't inp = 't Local.inp
  type 't lin_ = 't Linocaml_lin.EP.lin

  let[@inline] mklin x = Linocaml.({__lin=x})
  let[@inline] unlin x = Linocaml.(x.__lin)

  let[@inline] send sel v =
    {L.__m=(fun[@inline] lpre ->
       let ep = Local.send (sel (unlin lpre)) {Linocaml.data=v} in
       M.map (fun[@inline] v -> ((), mklin v)) ep)}

  let[@inline] deleg_send sel =
    {L.__m=(fun[@inline] lpre ->
       let subj, obj = lpre in
       let ep = Local.send (sel (unlin subj)) obj in
       M.map (fun[@inline] v -> ((), mklin v)) ep)}

  let[@inline] receive sel =
    {L.__m=(fun[@inline] lpre ->
       let ep = Local.receive (sel (unlin lpre)) in
       M.map (fun[@inline] v -> ((),mklin v)) ep)}

  let close =
    {L.__m=(fun[@inline] lpre ->
       Local.close (unlin lpre);
       M.return ((), {Linocaml.data=()}))}
end[@@inline]
