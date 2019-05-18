include Mpst_common

module M = struct
  type t = unit
end

include Guarded

type ('a,'b) out = {channel : 'a Event.channel ref; cont: 'b prot}
let unify a b = a := !b

let protclose _ = Close

let finish : ([`seq of close * 'a] as 'a) seq =
  let rec loop = lazy (Seq(val_ @@ protclose, SeqGuard(loop))) in
  Lazy.force loop

let choice_at : 'k 'ep 'ep_l 'ep_r 'g0_l 'g0_r 'g1 'g2.
                  (_, _, close, (< .. > as 'ep), 'g1 seq, 'g2 seq) role ->
                ('ep, < .. > as 'ep_l, < .. > as 'ep_r) obj_merge ->
                (_, _, 'ep_l, close, 'g0_l seq, 'g1 seq) role * 'g0_l seq ->
                (_, _, 'ep_r, close, 'g0_r seq, 'g1 seq) role * 'g0_r seq ->
                'g2 seq
  = fun r merge (r',g0left) (r'',g0right) ->
  let epL, epR =
    get r'.lens g0left,
    get r''.lens g0right in
  let g1left, g1right =
    put r'.lens g0left (val_ protclose),
    put r''.lens g0right (val_ protclose) in
  let g1 = seq_merge g1left g1right in
  let ep =
    val_ @@ (fun obj ->
      let oleft, oright = unprot_send epL, unprot_send epR in
      let oleft = oleft (map_option (fun objf -> merge.obj_splitL objf) obj)
      and oright = oright (map_option (fun objf -> merge.obj_splitR objf) obj) in
      merge.obj_merge oleft oright)
  in
  let g2 = put r.lens g1 ep
  in
  g2
                 
module MakeGlobal(X:LIN) = struct

  let merge_send label m1 m2 =
    let m1 = X.unlin (label.obj.call_obj m1) in
    let m2 = X.unlin (label.obj.call_obj m2) in
    unify m1.channel m2.channel;
    let cont = prot_merge m1.cont m2.cont in
    label.obj.make_obj (X.mklin {channel=m1.channel; cont})

  let merge_recv ev1 ev2 =
      Event.choose [ev1; ev2]

  let make_send rB lab (ph: _ Event.channel ref) epA =
    let mergefun = merge_send lab in
    let outobj = lab.obj.make_obj (X.mklin {channel=ph; cont=epA}) in
    val_ @@ make_mergeable mergefun rB.label outobj

  let make_recv rA lab ph epB =
    let ev = 
        Event.wrap
          (Event.guard (fun () -> Event.receive !ph))
          (fun v -> lab.var (v, X.mklin (unprot epB)))
    in
    val_ @@ make_mergeable merge_recv rA.label ev

  let ( --> ) : 'roleAVar 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
    (< .. > as 'roleAvar, 'labelvar Event.event, 'epA, 'roleBobj, 'g1, 'g2) role ->
    (< .. > as 'roleBobj, 'labelobj,             'epB, 'roleAvar, 'g0, 'g1) role ->
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
  unprot ep

let send {channel;cont} v = Event.sync (Event.send !channel v); unprot cont
let receive ev = Event.sync ev
let close _ = ()
