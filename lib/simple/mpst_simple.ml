include Mpst_common
type 'a one = One__
type 'a inp = 'a Event.event
type _ out =
  | Out : LinFlag.t * int * 'u Event.channel list ref * 't Mergeable.t -> ('u one * 't) out
  | OutMany : LinFlag.t * int * 'u Event.channel list ref * 't Mergeable.t -> ('u list * 't) out

let once (Out(o,_,_,_)) = o
let chan (Out(_,_,c,_)) = c
let cont (Out(_,_,_,d)) = d

let unify a b = a := !b

let rec toint : type a b c d. (a,b,c,d) Seq.lens -> int = function
  | Zero -> 0
  | Succ l -> toint l + 1

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
        Seq.get r'.role_index g0left,
        Seq.get r''.role_index g0right in
      let g1left, g1right =
        Seq.put r'.role_index g0left (Mergeable.make_no_merge [()]),
        Seq.put r''.role_index g0right (Mergeable.make_no_merge [()]) in
      let g1 = Seq.seq_merge g1left g1right in
      let ep = Mergeable.disjoint_merge merge epL epR
      in
      let g2 = Seq.put r.role_index g1 ep
      in
      g2)

module MakeGlobal(X:LIN) = struct

  let merge_in ev1 ev2 = Event.choose [ev1; ev2]

  let receive_one = function
    | [ch] -> Event.receive ch
    | _ -> assert false

  (* XXX a dumb implementation of receiving from multiple channels  *)
  let receive_list = function
    | ch::chs ->
       Event.wrap
         (Event.receive ch)
         (fun v ->
           v :: List.map (fun ch -> Event.sync (Event.receive ch)) chs)
    | [] -> failwith "no channel"

  let make_recv ~receive num rA lab (phss: _ Event.channel list ref list) epB =
    if num<=0 then begin
        failwith "make_recv: scatter/gather error: number of senders is <= 0"
      end;
    if List.length phss = 0 then begin
        failwith "make_recv: scatter/gather error: number of receivers is <= 0"
      end;
    assert (List.length !(List.hd phss) = num);
    let ev =
      List.init num
        (fun k -> fun once ->
          Event.wrap
            (Event.guard (fun () ->
                 LinFlag.use once;
                 let chs = List.map (fun phs -> List.nth !phs k) phss in
                 receive chs))
            (fun v -> lab.var (v, X.mklin (List.nth (Mergeable.out epB) k))))
    in
    let hook =
      lazy begin
          let eps = Mergeable.out epB in
          if num <> List.length eps then
            failwith "make_recv: endpoint count inconsistency; use unseq_param for scatter/gather"
        end
    in
    Mergeable.wrap_obj rA.role_label
      (Mergeable.make_with_hook
         hook
         merge_in
         ev)

  let merge_out : type u t. (u * t) out X.lin -> (u * t) out X.lin -> (u * t) out X.lin =
    fun out1 out2 ->
    let merge_  (o1,i1,s1,c1) (o2,i2,s2,c2) =
      assert (i1=i2);
      LinFlag.use o1; LinFlag.use o2;
      unify s1 s2;
      let o12 = LinFlag.create () in
      let c12 = Mergeable.merge c1 c2 in
      (o12, i1, s1, c12)
    in
    match X.unlin out1, X.unlin out2 with
    | Out(a1,b1,c1,d1), Out(a2,b2,c2,d2) ->
       let a3,b3,c3,d3 = merge_ (a1,b1,c1,d1) (a2,b2,c2,d2) in
       X.mklin @@ Out(a3,b3,c3,d3)
    | OutMany(a1,b1,c1,d1), OutMany(a2,b2,c2,d2) ->
       let a3,b3,c3,d3 = merge_ (a1,b1,c1,d1) (a2,b2,c2,d2) in
       X.mklin @@ OutMany(a3,b3,c3,d3)


  let send_one (a,b,c,d) = Out (a,b,c,d)
  let send_many (a,b,c,d) = OutMany (a,b,c,d)

  let make_send ~send num rB lab (phss: _ Event.channel list ref list) epA =
    if num<=0 then begin
        failwith "make_send: scatter/gather error: number of receivers is <= 0"
      end;
    if List.length phss = 0 then begin
        failwith "make_send: scatter/gather error: number of senders is <= 0"
      end;
    assert (List.length phss = num);
    let epA' =
      List.init num
        (fun k -> fun once ->
          X.mklin (send (once,k,List.nth phss k,epA)))
    in
    let hook =
      lazy begin
          let eps = Mergeable.out epA in
          if num <> List.length eps then
            failwith "make_send: endpoint count inconsistency; use unseq_param for scatter/gather"
        end
    in
    Mergeable.wrap_obj rB.role_label
      (Mergeable.wrap_obj lab.obj
         (Mergeable.make_with_hook
            hook
            merge_out
            epA'))

  let a2b anum bnum ~send ~receive = fun rA rB label g0 ->
    let ch =
      List.init anum (fun _ ->
          ref @@ List.init bnum (fun _ -> Event.new_channel ()))
    in
    let epB = Seq.get rB.role_index g0 in
    let ev  = make_recv ~receive bnum rA label ch epB in
    let g1  = Seq.put rB.role_index g0 ev
    in
    let epA = Seq.get rA.role_index g1 in
    let obj = make_send ~send anum rB label ch epA in
    let g2  = Seq.put rA.role_index g1 obj
    in g2

  let ( --> ) : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
    (< .. > as 'roleAobj, 'labelvar inp, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
    (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
    (< .. > as 'labelobj, [> ] as 'labelvar, ('v one * 'epA) out X.lin, 'v * 'epB X.lin) label ->
    'g0 seq -> 'g2 seq
    = fun rA rB label (Seq g0) ->
    Seq (fun env -> a2b 1 1 ~send:send_one ~receive:receive_one rA rB label (g0 env))

  let scatter : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
    (< .. > as 'roleAobj, 'labelvar inp, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
    (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
    (< .. > as 'labelobj, [> ] as 'labelvar, ('v list * 'epA) out X.lin, 'v * 'epB X.lin) label ->
    'g0 seq -> 'g2 seq
    = fun rA rB label (Seq g0) ->
    Seq (fun env ->
        if List.length env <= toint rB.role_index then begin
            failwith "use unseq_param [...] for scatter/gather"
          end;
        let bnum = List.nth env (toint rB.role_index) in
        a2b 1 bnum ~send:send_many ~receive:receive_one rA rB label (g0 env))

  let gather : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
    (< .. > as 'roleAobj, 'labelvar inp, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
    (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
    (< .. > as 'labelobj, [> ] as 'labelvar, ('v one * 'epA) out X.lin, 'v list * 'epB X.lin) label ->
    'g0 seq -> 'g2 seq
    = fun rA rB label (Seq g0) ->
    Seq (fun env ->
        if List.length env <= toint rB.role_index then begin
            failwith "use unseq_param [...] for scatter/gather"
          end;
        let anum = List.nth env (toint rA.role_index) in
        a2b anum 1 ~send:send_one ~receive:receive_list rA rB label (g0 env))
end

include MakeGlobal(struct type 'a lin = 'a let mklin x = x let unlin x = x end)

let send (Out(once,k,channel,cont)) v =
  assert (List.length !channel = 1);
  LinFlag.use once;
  Event.sync (Event.send (List.hd !channel) v);
  List.nth (Mergeable.out cont) k

let sendmany (OutMany(once,k,channels,cont)) vf =
  LinFlag.use once;
  List.iteri (fun i ch -> Event.sync (Event.send ch (vf i))) !channels;
  List.nth (Mergeable.out cont) k

let receive ev =
  Event.sync ev

let close _ = ()
