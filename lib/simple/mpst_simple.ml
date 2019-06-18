include Mpst_common

type _ out =
  Out : LinFlag.t * 'u Event.channel ref * int * int * 't Mergeable.t -> ('u * 't) out
type _ outmany =
  OutMany : LinFlag.t * 'u Event.channel ref list * 't Mergeable.t -> ('u * 't) outmany

let once (Out(o,_,_,_,_)) = o
let chan (Out(_,c,_,_,_)) = c
let cont (Out(_,_,_,_,d)) = d

let unify a b = a := !b

let finish : ([`cons of close * 'a] as 'a) Seq.t =
  SeqRepeat(Mergeable.make_no_merge Close)

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
    Seq.put r'.lens g0left (Mergeable.make_no_merge Close),
    Seq.put r''.lens g0right (Mergeable.make_no_merge Close) in
  let g1 = Seq.seq_merge g1left g1right in
  let ep = Mergeable.disjoint_merge merge epL epR
  in
  let g2 = Seq.put r.lens g1 ep
  in
  g2

module MakeGlobal(X:LIN) = struct

  let merge_in ev1 ev2 = Event.choose [ev1; ev2]

  let make_recv rA lab ph epB =
    let ev i =
      List.init i
        (fun k once ->
          Event.wrap
            (Event.guard (fun () -> LinFlag.use once; Event.receive !ph))
            (fun v -> lab.var (v, X.mklin (List.nth (Mergeable.out epB i) k))))
    in
    Mergeable.wrap_obj rA.label
      (Mergeable.make_with_hook
         (* (lazy (Mergeable.resolve_merge epB)) *)
         (lazy ()) (* FIXME *)
         merge_in
         ev)

  let merge_out = fun out1 out2 ->
    let (Out(o1,s1,i1,n1,c1), Out(o2,s2,i2,n2,c2)) = X.unlin out1, X.unlin out2 in
    assert (i1=i2);
    assert (n1=n2);
    LinFlag.use o1; LinFlag.use o2;
    unify s1 s2;
    let o12 = LinFlag.create () in
    let c12 = Mergeable.merge c1 c2 in
    X.mklin (Out(o12, s1, i1, n1, c12))

  let make_send rB lab (ph: _ Event.channel ref) epA =
    let epA' n =
      List.init n
        (fun i once ->
          X.mklin (Out(once,ph,i,n,epA)))
    in
    Mergeable.wrap_obj rB.label
      (Mergeable.wrap_obj lab.obj
         (Mergeable.make_with_hook
            (* (lazy (Mergeable.resolve_merge epA)) *)
            (lazy ()) (* FIXME *)
            merge_out
            epA'))

  let gen_channels sh rh =
    Thread.create (fun () ->
        let sn,srh = Event.sync (Event.receive sh) in
        let rn,rrh = Event.sync (Event.receive rh) in
        let chs = List.init sn (fun si ->
            List.init rn (fun ri ->
                ref (Event.new_channel ())))
        in
        Event.sync (Event.send srh chs);
        Event.sync (Event.send rrh @@ Mpst_base.transpose chs);

  let ( --> ) : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
    (< .. > as 'roleAobj, 'labelvar Event.event, 'epA, 'roleBobj, 'g1, 'g2) role ->
    (< .. > as 'roleBobj, 'labelobj,             'epB, 'roleAobj, 'g0, 'g1) role ->
    (< .. > as 'labelobj, [> ] as 'labelvar, ('v * 'epA) out X.lin, 'v * 'epB X.lin) label ->
    'g0 -> 'g2
    = fun rA rB label g0 ->
    let chs = Event.new_channel ()
    in
    let epB = Seq.get rB.lens g0 in
    let ev  = make_recv rA label ch epB in
    let g1  = Seq.put rB.lens g0 ev
    in
    let epA = Seq.get rA.lens g1 in
    let obj = make_send rB label ch epA in
    let g2  = Seq.put rA.lens g1 obj
    in g2
     
  (* let scatter : 'rAobj 'labelvar 'epA 'rBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
   *              (< .. > as 'rAobj, 'labelvar Event.event, 'epA, 'rBobj, 'g1, 'g2) role ->
   *              (< .. > as 'rBobj, 'labelobj, 'epB list, 'rAobj list, 'g0, 'g1) role ->
   *              (< .. > as 'labelobj, [> ] as 'labelvar, ('v * 'epA) outmany X.lin, 'v * 'epB X.lin) label -> 'g0 -> 'g2
   *   = fun rA rB label g0 ->
   *   let epBs = Seq.get rB.lens g0 in
   *   let chs = lazy (List.map (fun _ -> ref (Event.new_channel ())) (Mergeable.out epBs)) in
   *   let epBs = lazy (List.map2 (make_recvone rA label) (Lazy.force chs) (Mergeable.out epBs)) in
   *   let g1  = Seq.put rB.lens g0 (Mergeable.make (fun x _ -> x) (fun _ -> Lazy.force epBs))
   *   in
   *   let epA = Seq.get rA.lens g1 in
   *   let epA = make_sendmany rB label chs epA in
   *   Seq.put rA.lens g1 epA *)

end
include MakeGlobal(struct type 'a lin = 'a let mklin x = x let unlin x = x end)

let send (Out(once,channel,i,n,cont)) v =
  LinFlag.use once;
  Event.sync (Event.send !channel v);
  List.nth (Mergeable.out cont n) i
let receive ev =
  Event.sync ev
let close _ = ()

(*
ideal structure for scatter-gather
(a ->> b) msg @@ finish

let ab = 
  let wait, push = Lwt.task ()
  let ea f =
    let chs = wait () in
    object
      method role_B =
        object method msg =
          Out(f,chs,cont)
        end
    end
  let eb fs =
     let chs = push (List.map (fun _ -> Event.new_channel ()) fs) in
     push 
     object method role_A =

*)
