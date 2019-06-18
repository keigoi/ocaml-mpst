include Mpst_common

type _ out =
  Out : LinFlag.t * 'u Event.channel ref * int * int * 't Mergeable.t -> ('u * 't) out
type _ outmany =
  OutMany : LinFlag.t * 'u Event.channel ref list * 't Mergeable.t -> ('u * 't) outmany

let once (Out(o,_,_,_,_)) = o
let chan (Out(_,c,_,_,_)) = c
let cont (Out(_,_,_,_,d)) = d

let unify a b = a := !b

let finish : ([`cons of close * 'a] as 'a) seq =
  Seq (fun _ -> SeqRepeat(Mergeable.make_no_merge [Close])) (* FIXME *)

let choice_at : 'ep 'ep_l 'ep_r 'g0_l 'g0_r 'g1 'g2.
                  (_, _, unit, (< .. > as 'ep), 'g1 Seq.t, 'g2 Seq.t) role ->
                ('ep, < .. > as 'ep_l, < .. > as 'ep_r) obj_merge ->
                (_, _, 'ep_l, unit, 'g0_l Seq.t, 'g1 Seq.t) role * 'g0_l seq ->
                (_, _, 'ep_r, unit, 'g0_r Seq.t, 'g1 Seq.t) role * 'g0_r seq ->
                'g2 seq
  = fun r merge (r',Seq g0left) (r'',Seq g0right) ->
  Seq (fun env ->
      let g0left, g0right = g0left env, g0right env in
      let epL, epR =
        Seq.get r'.lens g0left,
        Seq.get r''.lens g0right in
      let g1left, g1right =
        Seq.put r'.lens g0left (Mergeable.make_no_merge [()]),
        Seq.put r''.lens g0right (Mergeable.make_no_merge [()]) in
      let g1 = Seq.seq_merge g1left g1right in
      let ep = Mergeable.disjoint_merge merge epL epR
      in
      let g2 = Seq.put r.lens g1 ep
      in
      g2)

module MakeGlobal(X:LIN) = struct

  let merge_in ev1 ev2 = Event.choose [ev1; ev2]

  let make_recv num rA lab ph epB =
    let ev num =
      List.init num
        (fun k once ->
          Event.wrap
            (Event.guard (fun () -> LinFlag.use once; Event.receive !ph))
            (fun v -> lab.var (v, X.mklin (List.nth (Mergeable.out epB num) k))))
    in
    Mergeable.wrap_obj rA.label
      (Mergeable.make_with_hook
         (lazy (Mergeable.resolve_merge epB num))
         merge_in
         (ev num))

  let merge_out = fun out1 out2 ->
    let (Out(o1,s1,i1,n1,c1), Out(o2,s2,i2,n2,c2)) = X.unlin out1, X.unlin out2 in
    assert (i1=i2);
    assert (n1=n2);
    LinFlag.use o1; LinFlag.use o2;
    unify s1 s2;
    let o12 = LinFlag.create () in
    let c12 = Mergeable.merge c1 c2 in
    X.mklin (Out(o12, s1, i1, n1, c12))

  let make_send num rB lab (ph: _ Event.channel ref) epA =
    let epA' num =
      List.init num
        (fun i once ->
          X.mklin (Out(once,ph,i,num,epA)))
    in
    Mergeable.wrap_obj rB.label
      (Mergeable.wrap_obj lab.obj
         (Mergeable.make_with_hook
            (lazy (Mergeable.resolve_merge epA num))
            merge_out
            (epA' num)))

  (* let gen_channels sh rh =
   *   Thread.create (fun () ->
   *       let sn,srh = Event.sync (Event.receive sh) in
   *       let rn,rrh = Event.sync (Event.receive rh) in
   *       let chs = List.init sn (fun si ->
   *           List.init rn (fun ri ->
   *               ref (Event.new_channel ())))
   *       in
   *       Event.sync (Event.send srh chs);
   *       Event.sync (Event.send rrh @@ Mpst_base.transpose chs) *)

  let ( --> ) : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
    (< .. > as 'roleAobj, 'labelvar Event.event, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
    (< .. > as 'roleBobj, 'labelobj,             'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
    (< .. > as 'labelobj, [> ] as 'labelvar, ('v * 'epA) out X.lin, 'v * 'epB X.lin) label ->
    'g0 seq -> 'g2 seq
    = fun rA rB label (Seq g0) ->
    Seq (fun env ->
        let g0 = g0 env in
        let ch = ref (Event.new_channel ())
        in
        let epB = Seq.get rB.lens g0 in
        let ev  = make_recv 1 rA label ch epB in
        let g1  = Seq.put rB.lens g0 ev
        in
        let epA = Seq.get rA.lens g1 in
        let obj = make_send 1 rB label ch epA in
        let g2  = Seq.put rA.lens g1 obj
        in g2)

  (* let make_recv_mult rA lab ph epB =
   *   let ev once =
   *     Event.wrap
   *       (Event.guard (fun () -> LinFlag.use once; Event.receive !ph))
   *       (fun v -> lab.var (v, X.mklin (Mergeable.out epB)))
   *   in
   *   Mergeable.wrap_obj rA.label
   *     (Mergeable.make_with_hook
   *        (lazy (Mergeable.resolve_merge epB))
   *        merge_in
   *        ev)
   * 
   * let ( --> ) : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
   *   (< .. > as 'roleAobj, 'labelvar Event.event, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
   *   (< .. > as 'roleBobj, 'labelobj,             'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
   *   (< .. > as 'labelobj, [> ] as 'labelvar, ('v * 'epA) out X.lin, 'v * 'epB X.lin) label ->
   *   'g0 seq -> 'g2 seq
   *   = fun rA rB label (Seq g0) ->
   *   Seq (fun env ->
   *       let g0 = g0 env in
   *       let ch = ref (Event.new_channel ())
   *       in
   *       let epB = Seq.get rB.lens g0 in
   *       let ev  = make_recv rA label ch epB in
   *       let g1  = Seq.put rB.lens g0 ev
   *       in
   *       let epA = Seq.get rA.lens g1 in
   *       let obj = make_send rB label ch epA in
   *       let g2  = Seq.put rA.lens g1 obj
   *       in g2)
   * 
   * let rec toint : type a b c d. (a,b,c,d) Seq.lens -> int = function
   *   | Zero -> 0
   *   | Succ l -> toint l + 1
   * 
   * let scatter : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
   *   (< .. > as 'roleAobj, 'labelvar Event.event, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
   *   (< .. > as 'roleBobj, 'labelobj,             'epB list, 'roleAobj list, 'g0 Seq.t, 'g1 Seq.t) role ->
   *   (< .. > as 'labelobj, [> ] as 'labelvar, ('v * 'epA) out X.lin, 'v * 'epB X.lin) label ->
   *   'g0 seq -> 'g2 seq
   *   = fun rA rB label (Seq g0) ->
   *   Seq (fun env ->
   *       let g0 = g0 env in
   *       let rBidx = toint rB.lens in
   *       let cnt = List.nth env rBidx(\*FIXME*\) in
   *       let chs = List.init cnt (fun _ -> ref (Event.new_channel ())) in
   *       let epB = Seq.get rB.lens g0 in
   *       let ev  = make_recv_mult rA label chs epB in
   *       let g1  = Seq.put rB.lens g0 ev
   *       in
   *       let epA = Seq.get rA.lens g1 in
   *       let obj = make_sendmany rB label chs epA in
   *       let g2  = Seq.put rA.lens g1 obj
   *       in g2) *)
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
