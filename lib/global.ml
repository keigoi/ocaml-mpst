open Base
include Global_common
open Local

type 'g global = (pipe, 'g) Global_common.t

module type LIN = sig
  type 'a lin
  val mklin : 'a -> 'a lin
  val unlin : 'a lin -> 'a
end

module MakeGlobal(X:LIN) = struct

  let receive_one once k phss wrapfun =
    match List.hd phss with
    | BareOutChan chs ->
       Ev (once, Event.wrap (Event.receive (List.nth !chs k)) wrapfun)
    | BareOutIPC (tag,chs) ->
       PipeIn (once, [List.nth chs k],
               [(tag, (fun ps -> let {inp; _} = List.hd ps in wrapfun (input_value inp)))])

  (* XXX a dumb implementation of receiving from multiple channels  *)
  let receive_list once k phss wrapfun =
    let make_ev_ = function
      | BareOutChan chs ->
         Event.guard (fun () ->
              Event.receive (List.nth !chs k))
      | _ -> assert false
    and pipein_ = function
      | BareOutIPC (_,chs) -> List.nth chs k
      | _ -> assert false
    in
    match phss with
    | ((BareOutChan _) as ch :: chs) ->
       let evs = List.map make_ev_ chs
       and ev = make_ev_ ch in
       Ev (once, Event.wrap ev (fun v -> wrapfun (v :: List.map Event.sync evs)))
    | (BareOutIPC (tag,_) :: _) as chs ->
       let ps = List.map pipein_ chs in
       PipeIn (once, ps, [(tag, (fun ps -> wrapfun (List.map (fun {inp;_} -> input_value inp) ps)))])
    | [] ->
       failwith "no channel"

  let make_recv ~receive num rA lab phss epB =
    if num<=0 then begin
        failwith "make_recv: scatter/gather error: number of senders is <= 0"
      end;
    if List.length phss = 0 then begin
        failwith "make_recv: scatter/gather error: number of receivers is <= 0"
      end;
    let wrapfun k v = lab.var (v, X.mklin (List.nth (Mergeable.out epB) k))
    in
    let ev = List.init num (fun k -> fun once -> receive once k phss (wrapfun k))
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
         Local.merge_in
         ev)


  let send_one (a,b,(c,d)) = Local.Out (a,b,(c,d))
  let send_many (a,b,(c,d)) = Local.OutMany (a,b,(c,d))

  let make_send ~send num rB lab (phss: _ bareout list) epA =
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
                  X.mklin (send (once,List.nth phss k,(k,epA))))
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
            (fun o1 o2 -> X.mklin (Local.merge_out (X.unlin o1) (X.unlin o2)))
            epA'))

  let makeipc env hs yourid youcnt =
    List.map (fun arr ->
        let rb = Seq.int_of_lens yourid in
        let chs =
          match arr.(rb) with
          | None ->
             let chs = List.init youcnt (fun _ -> new_pipe ()) in
             arr.(rb) <- Some (chs);
             chs
          | Some(chs) -> chs
        in
        chs) hs

  let updateipc srckts srcidx dstidx = function
    | Local -> ()
    | IPCProcess kts ->
       let kss = List.map (fun srckt -> Conn_table.get srckt dstidx) srckts in
       let kss = transpose kss in
       List.iter2 (fun kt ks -> Conn_table.put kt srcidx ks) kts kss

  let a2b env ?num_senders ?num_receivers ~send ~receive = fun rA rB label g0 ->
    let anum = of_option num_senders ~dflt:(multiplicity env rA.role_index) in
    let bnum = of_option num_receivers ~dflt:(multiplicity env rB.role_index) in
    let ch =
      match kind env rA.role_index, kind env rB.role_index with
      | Local, Local ->
         List.init anum (fun _ ->
             BareOutChan(ref @@ List.init bnum (fun _ -> Event.new_channel ())))
      | IPCProcess kts, bkind ->
         assert (List.length kts = anum);
         let chss = List.map (fun kt -> Conn_table.get_or_create kt rB.role_index bnum) kts in
         updateipc kts rA.role_index rB.role_index bkind;
         List.map (fun chs -> BareOutIPC(mktag label.var, chs)) chss
      | akind, IPCProcess kts ->
         assert (List.length kts = bnum);
         let chss = List.map (fun kt -> Conn_table.get_or_create kt rA.role_index anum) kts in
         updateipc kts rB.role_index rA.role_index akind;
         List.map (fun chs -> BareOutIPC(mktag label.var, chs)) chss
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
                (< .. > as 'roleAobj, 'labelvar Local.inp, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
                (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
                (< .. > as 'labelobj, [> ] as 'labelvar, ('v one * 'epA) Local.out X.lin, 'v * 'epB X.lin) label ->
                'g0 global -> 'g2 global
    = fun rA rB label (Seq g0) ->
    Seq (fun env -> a2b env ~num_senders:1 ~num_receivers:1 ~send:send_one ~receive:receive_one rA rB label (g0 env))

  let scatter : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
                (< .. > as 'roleAobj, 'labelvar Local.inp, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
                (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
                (< .. > as 'labelobj, [> ] as 'labelvar, ('v list * 'epA) Local.out X.lin, 'v * 'epB X.lin) label ->
                'g0 global -> 'g2 global
    = fun rA rB label (Seq g0) ->
    Seq (fun env ->
        if List.length env.props <= Seq.int_of_lens rB.role_index then begin
            failwith "use unseq_param [...] for scatter/gather"
          end;
        a2b env ~num_senders:1 ~send:send_many ~receive:receive_one rA rB label (g0 env))

  let gather : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
               (< .. > as 'roleAobj, 'labelvar Local.inp, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
               (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
               (< .. > as 'labelobj, [> ] as 'labelvar, ('v one * 'epA) Local.out X.lin, 'v list * 'epB X.lin) label ->
               'g0 global -> 'g2 global
    = fun rA rB label (Seq g0) ->
    Seq (fun env ->
        if List.length env.props <= Seq.int_of_lens rB.role_index then begin
            failwith "use unseq_param [...] for scatter/gather"
          end;
        a2b env ~num_receivers:1 ~send:send_one ~receive:receive_list rA rB label (g0 env))
end

include MakeGlobal(struct type 'a lin = 'a let mklin x = x let unlin x = x end)

let gen g =
  Global_common.gen_with_param
    {props=[]} g

let gen_with_param p g =
  Global_common.gen_with_param
    {props=List.map (fun p -> {multiplicity=p; kind=Local}) p}
    g

let gen_with_param_ipc p g =
  Global_common.gen_with_param
    {props=List.map (fun p ->
               {multiplicity=p;
                kind=IPCProcess (List.init p (fun _ -> Conn_table.create new_pipe))}) p}
    g
