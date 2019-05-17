include Mpst_common

module M = struct
  type t = unit
end

include Seq(M)
include Guarded

type ('a,'b) out = {channel : 'a Event.channel ref; cont: 'b prot}
let unify a b = a := !b
                 
module MakeGlobal(X:LIN) = struct

  let make_send rB lab (ph: _ Event.channel ref) epA =
    let method_ obj kt =
      match obj with
      | None ->
         X.mklin {channel=ph; cont=epA}
      | Some obj ->
         let obj = obj kt in
         let out' = X.unlin (lab.call_obj (rB.label.call_obj obj)) in
         let ph', epA' = out'.channel, out'.cont in
         unify ph ph';
         let epA = prot_merge epA epA' in
         X.mklin {channel=ph; cont=epA}
    in
    (* <role_rB : < lab : v -> epA > > *)
    val_ @@ ProtSend (fun obj kt ->
        rB.label.make_obj (lab.make_obj (method_ obj kt)))

  let make_recv rA lab ph epB =
    let method_ obj kt =
      let ev cont =
        Event.wrap
          (Event.guard (fun () -> Event.receive !ph))
          (fun v -> lab.make_var (v, X.mklin (unprot kt cont)(*FIXME*)))
      in
      match obj with
      | None ->
         ev epB
      | Some obj ->
         let obj = obj kt in
         Event.choose [ev epB; rA.label.call_obj obj]
    in
    val_ @@ ProtRecv (fun obj kt ->
        rA.label.make_obj (method_ obj kt))

  let ( --> ) : 'roleAVar 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
    (< .. > as 'roleAvar, _, 'labelvar Event.event, 'epA, 'roleBobj, 'g1, 'g2) role ->
    (< .. > as 'roleBobj, _, 'labelobj,             'epB, 'roleAvar, 'g0, 'g1) role ->
    (< .. > as 'labelobj, [> ] as 'labelvar, ('v, 'epA) out X.lin, 'v * 'epB X.lin) label ->
    'g0 -> 'g2
    = fun rA rB label g0 ->
    let ch = ref (Event.new_channel ())
    in
    let epB = get rB.lens g0 in
    let ev  = make_recv rA label ch epB in
    let g1  = put rB.lens g0 ev
    in
    let epA = get rA.lens g1 in
    let obj = make_send rB label ch epA in
    let g2  = put rA.lens g1 obj
    in g2
end
      
include MakeGlobal(struct type 'a lin = 'a let mklin x = x let unlin x = x end)

let get_ep r g =
  let ep = get r.lens g in
  unprot () ep

let send {channel;cont} v = Event.sync (Event.send !channel v); unprot () cont
let receive ev = Event.sync ev
let close Close = ()
