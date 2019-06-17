include Mpst_common

type _ out =
  Out : LinFlag.t * 'u Event.channel ref * 't Mergeable.t -> ('u * 't) out

let once (Out(o,_,_)) = o
let chan (Out(_,c,_)) = c
let cont (Out(_,_,d)) = d

let unify a b = a := !b

let finish : ([`cons of close * 'a] as 'a) Seq.t =
  SeqRepeat(Mergeable.make_no_merge (fun _ -> Close))

let choice_at : 'ep 'ep_l 'ep_r 'g0_l 'g0_r 'g1 'g2.
                  (_, _, close, (< .. > as 'ep), 'g1 Seq.t, 'g2 Seq.t) role ->
                ('ep, < .. > as 'ep_l, < .. > as 'ep_r) obj_merge ->
                (_, _, 'ep_l, close, 'g0_l Seq.t, 'g1 Seq.t) role * 'g0_l Seq.t ->
                (_, _, 'ep_r, close, 'g0_r Seq.t, 'g1 Seq.t) role * 'g0_r Seq.t ->
                'g2 Seq.t
  = fun r merge (r',g0left) (r'',g0right) ->
  let epL, epR =
    Seq.get r'.lens g0left,
    Seq.get r''.lens g0right in
  let g1left, g1right =
    Seq.put r'.lens g0left (Mergeable.make_no_merge (fun _ -> Close)),
    Seq.put r''.lens g0right (Mergeable.make_no_merge (fun _ -> Close)) in
  let g1 = Seq.seq_merge g1left g1right in
  let ep = Mergeable.disjoint_merge merge epL epR
  in
  let g2 = Seq.put r.lens g1 ep
  in
  g2

module MakeGlobal(X:LIN) = struct

  let merge_out = fun out1 out2 ->
    let (Out(o1,s1,c1), Out(o2,s2,c2)) = X.unlin out1, X.unlin out2 in
    LinFlag.use o1; LinFlag.use o2;
    unify s1 s2;
    let o12 = LinFlag.create () in
    let c12 = Mergeable.merge c1 c2 in
    X.mklin (Out(o12, s1, c12))

  let make_send rB lab (ph: _ Event.channel ref) epA =
    let epA' once =
      X.mklin (Out(once,ph,epA))
    in
    Mergeable.wrap_obj rB.label
      (Mergeable.wrap_obj lab.obj
         (Mergeable.make_with_hook
            (lazy (Mergeable.resolve_merge epA))
            merge_out
            epA'))

  let merge_in ev1 ev2 = Event.choose [ev1; ev2]

  let make_recv rA lab ph epB =
    let ev once =
      Event.wrap
        (Event.guard (fun () -> LinFlag.use once; Event.receive !ph))
        (fun v -> lab.var (v, X.mklin (Mergeable.out epB)))
    in
    Mergeable.wrap_obj rA.label
      (Mergeable.make_with_hook
         (lazy (Mergeable.resolve_merge epB))
         merge_in
         ev)

  let ( --> ) : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
    (< .. > as 'roleAobj, 'labelvar Event.event, 'epA, 'roleBobj, 'g1, 'g2) role ->
    (< .. > as 'roleBobj, 'labelobj,             'epB, 'roleAobj, 'g0, 'g1) role ->
    (< .. > as 'labelobj, [> ] as 'labelvar, ('v * 'epA) out X.lin, 'v * 'epB X.lin) label ->
    'g0 -> 'g2
    = fun rA rB label g0 ->
    let ch = ref (Event.new_channel ())
    in
    let epB = Seq.get rB.lens g0 in
    let ev  = make_recv rA label ch epB in
    let g1  = Seq.put rB.lens g0 ev
    in
    let epA = Seq.get rA.lens g1 in
    let obj = make_send rB label ch epA in
    let g2  = Seq.put rA.lens g1 obj
    in g2
end

include MakeGlobal(struct type 'a lin = 'a let mklin x = x let unlin x = x end)

let send (Out(once,channel,cont)) v =
  LinFlag.use once;
  Event.sync (Event.send !channel v);
  Mergeable.out cont
let receive ev =
  Event.sync ev
let close _ = ()
