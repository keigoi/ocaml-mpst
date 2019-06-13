include Mpst_common

module M = struct
  type t = unit
end
type _ out =
  Chan : 'a Event.channel ref * 'b Mergeable.t -> ('a * 'b) out

let chan (Chan(c,_)) = c
let cont (Chan(_,d)) = d

let unify a b = a := !b

let finish : ([`cons of close * 'a] as 'a) Seq.t =
  SeqRepeat(Mergeable.no_merge Close)

let choice_at : 'k 'ep 'ep_l 'ep_r 'g0_l 'g0_r 'g1 'g2.
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
    Seq.put r'.lens g0left (Mergeable.no_merge Close),
    Seq.put r''.lens g0right (Mergeable.no_merge Close) in
  let g1 = Seq.seq_merge g1left g1right in
  let ep =
    Mergeable.bare_ @@ (fun obj ->
      let oleft, oright = Mergeable.out epL, Mergeable.out epR in
      let oleft = oleft (map_option (fun objf -> merge.obj_splitL objf) obj)
      and oright = oright (map_option (fun objf -> merge.obj_splitR objf) obj) in
      merge.obj_merge oleft oright)
  in
  let g2 = Seq.put r.lens g1 ep
  in
  g2

module MakeGlobal(X:LIN) = struct

  let make_send rB lab (ph: _ Event.channel ref) epA =
    let m1 = Chan(ph,epA) in
    let outobj = lab.obj.make_obj (X.mklin m1) in
    Mergeable.Val (fun obj ->
        match obj with
        | None ->
           ignore (Mergeable.resolve_merge epA);
           rB.label.make_obj outobj
        | Some obj2 ->
           let m2 = X.unlin (lab.obj.call_obj (rB.label.call_obj obj2)) in
           unify (chan m1) (chan m2);
           let k = Mergeable.merge (cont m1) (cont m2) in
           ignore (Mergeable.resolve_merge k);
           rB.label.make_obj
             (lab.obj.make_obj
                (X.mklin (Chan(chan m1, k)))))

  let make_recv rA lab ph epB =
    let ev =
        Event.wrap
          (Event.guard (fun () -> Event.receive !ph))
          (fun v -> lab.var (v, X.mklin (Mergeable.out_ epB)))
    in
    Mergeable.Val (fun obj ->
        ignore (Mergeable.resolve_merge epB);
        match obj with
        | None ->
           rA.label.make_obj ev
        | Some obj2 ->
           let ev2 = rA.label.call_obj obj2 in
           rA.label.make_obj (Event.choose [ev; ev2]))

  let ( --> ) : 'roleAVar 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
    (< .. > as 'roleAvar, 'labelvar Event.event, 'epA, 'roleBobj, 'g1, 'g2) role ->
    (< .. > as 'roleBobj, 'labelobj,             'epB, 'roleAvar, 'g0, 'g1) role ->
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

let send (Chan(channel,cont)) v = Event.sync (Event.send !channel v); Mergeable.out_ cont
let receive ev = Event.sync ev
let close _ = ()
