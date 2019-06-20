open Base
open Local

include Global_common

module type LIN = sig
  type 'a lin
  val mklin : 'a -> 'a lin
  val unlin : 'a lin -> 'a
end

type pipe = {inp: in_channel; out: out_channel}
type dpipe = {me:pipe; otr:pipe; dir: int}

let swap_dpipe {me=otr;otr=me;dir} = {me;otr;dir= -dir}

let new_dpipe () =
  let my_inp, otr_out = Unix.pipe () in
  let otr_inp, my_out = Unix.pipe () in
  {me={inp=Unix.in_channel_of_descr my_inp; out=Unix.out_channel_of_descr my_out};
   otr={inp=Unix.in_channel_of_descr otr_inp; out=Unix.out_channel_of_descr otr_out}; dir=1}

module MakeGlobal
         (* (M:S.MONAD)(Event:S.EVENT with type 'a monad = 'a M.t) *)
         (X:LIN) = struct
  module Pure = struct
    type 'a t = 'a
    let return a = a
    let return_unit = ()
    let bind x f = f x
    let iteriM = List.iteri
  end
  module Event = struct
    include Event
    type 'a monad = 'a
  end

  module L = Local.Make(Pure)(Event)
  open L

  type 'v chan =
    | Bare of 'v Event.channel list ref
    | IPC of tag * dpipe list

  let bare_of_chan = function
    | Bare xs ->
       BareOutChan xs
    | IPC (tag, chs) ->
       BareOutIPC (List.map (fun {me={out;_};_} ->
                       (fun v ->
                         output_value out tag;
                         output_value out v;
                         flush out))
                     chs)

  type 'g global = (dpipe, 'g) Global_common.t

  let make_inp_one out_chs myidx wrapfun =
    match List.hd out_chs with
    | Bare chs ->
       fun once ->
       InpChan (once, Event.wrap (Event.receive (List.nth !chs myidx)) wrapfun)
    | IPC (tag,chs) ->
       let {me={inp=ch;_};_} = swap_dpipe (List.nth chs myidx) in
       fun once ->
       InpIPC
         (once,
          Event.guard (fun () -> Event.always (input_value ch)),
          [(tag, Event.wrap (Event.guard (fun () -> Event.always @@ input_value ch)) wrapfun)])

  (* XXX a dumb implementation of receiving from multiple channels  *)
  let make_inp_list out_chs myidx wrapfun =
    match out_chs with
    | ((Bare _) as ch :: chs) ->
       let make_ev_ = function
         | Bare chs ->
            Event.guard (fun () -> Event.receive (List.nth !chs myidx))
         | _ -> assert false
       in
       let evs = List.map make_ev_ chs
       and ev = make_ev_ ch in
       fun once ->
       InpChan (once, Event.wrap ev (fun v -> wrapfun (v :: List.map Event.sync evs)))
    | (IPC (tag,_) :: _) as chs ->
       let pipein_ = function
         | IPC (_,chs) -> List.nth chs myidx
         | _ -> assert false
       in
       let chs = List.map pipein_ chs in
       let chs = List.map swap_dpipe chs in
       let chs = List.map (fun {me={inp;_};_} -> inp) chs in
       fun once ->
       InpIPC (once, Event.guard (fun () -> Event.always (List.hd @@ List.map input_value chs)),
               [(tag, Event.wrap (Event.guard (fun () ->
                                      Event.always @@ List.map input_value chs))
                        wrapfun)])
    | [] ->
       failwith "no channel"

  type inpkind = InpOne | InpList

  let make_recv ~make_inp num_receivers rA lab out_chs epB =
    if num_receivers=0 then begin
        failwith "make_recv: scatter/gather error: number of receivers is zero"
      end;
    if List.length out_chs = 0 then begin
        failwith "make_recv: scatter/gather error: number of senders is zero"
      end;
    let bare_inp_chs =
      List.init num_receivers (fun myidx ->
          let wrapfun v = lab.var (v, X.mklin (List.nth (Mergeable.out epB) myidx))
          in
          make_inp out_chs myidx wrapfun)
    in
    let hook =
      lazy begin
          (* force the following endpoints *)
          let eps = Mergeable.out epB in
          if num_receivers <> List.length eps then
            failwith "make_recv: endpoint count inconsistency"
        end
    in
    Mergeable.wrap_obj rA.role_label
      (Mergeable.make_with_hook
         hook
         L.merge_in
         bare_inp_chs)


  let make_out_one (a,b,(c,d)) = L.Out (a,b,(c,d))
  let make_out_list (a,b,(c,d)) = L.OutMany (a,b,(c,d))

  let make_send ~make_out num_senders rB lab (chs: _ chan list) epA =
    if num_senders = 0 then begin
        failwith "make_send: scatter/gather error: number of senders is = 0"
      end;
    if List.length chs = 0 then begin
        failwith "make_send: scatter/gather error: number of senders is = 0"
      end;
    assert (List.length chs = num_senders);
    let epA' =
      List.init num_senders
        (fun k ->
          let ch = bare_of_chan (List.nth chs k) in
          fun once -> X.mklin (make_out (once,ch,(k,epA))))
    in
    let hook =
      lazy begin
          let eps = Mergeable.out epA in
          if num_senders <> List.length eps then
            failwith "make_send: endpoint count inconsistency"
        end
    in
    Mergeable.wrap_obj rB.role_label
      (Mergeable.wrap_obj lab.obj
         (Mergeable.make_with_hook
            hook
            (fun o1 o2 -> X.mklin (L.merge_out (X.unlin o1) (X.unlin o2)))
            epA'))

  let updateipc srckts srcidx dstidx = function
    | EpLocal -> ()
    | EpIPCProcess kts ->
       let kss = List.map (fun srckt -> Table.get srckt dstidx) srckts in
       let kss = transpose kss in
       let swap ks = List.map swap_dpipe ks in
       List.iter2 (fun kt ks -> Table.put kt srcidx (swap ks)) kts kss

  let a2b env ?num_senders ?num_receivers ~make_out ~make_inp = fun rA rB label g0 ->
    let anum = of_option num_senders ~dflt:(multiplicity env rA.role_index) in
    let bnum = of_option num_receivers ~dflt:(multiplicity env rB.role_index) in
    let chs =
      match epkind env rA.role_index, epkind env rB.role_index with
      | EpLocal, EpLocal ->
         List.init anum (fun _ ->
             Bare(ref @@ List.init bnum (fun _ -> Event.new_channel ())))
      | EpIPCProcess kts, bkind ->
         assert (List.length kts = anum);
         let chss = List.map (fun kt -> Table.get_or_create kt rB.role_index bnum) kts in
         updateipc kts rA.role_index rB.role_index bkind;
         List.map (fun chs -> IPC(make_tag label.var, chs)) chss
      | akind, EpIPCProcess kts ->
         assert (List.length kts = bnum);
         let chss = List.map (fun kt -> Table.get_or_create kt rA.role_index anum) kts in
         let chss = transpose chss in
         updateipc kts rB.role_index rA.role_index akind;
         List.map (fun chs -> IPC(make_tag label.var, List.map swap_dpipe chs)) chss
    in
    let epB = Seq.get rB.role_index g0 in
    let ev  = make_recv bnum ~make_inp rA label chs epB in
    let g1  = Seq.put rB.role_index g0 ev
    in
    let epA = Seq.get rA.role_index g1 in
    let obj = make_send ~make_out anum rB label chs epA in
    let g2  = Seq.put rA.role_index g1 obj
    in g2

  let ( --> ) : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
                (< .. > as 'roleAobj, 'labelvar L.inp, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
                (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
                (< .. > as 'labelobj, [> ] as 'labelvar, ('v one * 'epA) L.out X.lin, 'v * 'epB X.lin) label ->
                'g0 global -> 'g2 global
    = fun rA rB label (Seq g0) ->
    Seq (fun env ->
        a2b env ~num_senders:1 ~num_receivers:1 ~make_out:make_out_one ~make_inp:make_inp_one rA rB label (g0 env))

  let scatter : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
                (< .. > as 'roleAobj, 'labelvar L.inp, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
                (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
                (< .. > as 'labelobj, [> ] as 'labelvar, ('v list * 'epA) L.out X.lin, 'v * 'epB X.lin) label ->
                'g0 global -> 'g2 global
    = fun rA rB label (Seq g0) ->
    Seq (fun env ->
        a2b env ~num_senders:1 ~make_out:make_out_list ~make_inp:make_inp_one rA rB label (g0 env))

  let gather : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
               (< .. > as 'roleAobj, 'labelvar L.inp, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
               (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
               (< .. > as 'labelobj, [> ] as 'labelvar, ('v one * 'epA) L.out X.lin, 'v list * 'epB X.lin) label ->
               'g0 global -> 'g2 global
    = fun rA rB label (Seq g0) ->
    Seq (fun env ->
        a2b env ~num_receivers:1 ~make_out:make_out_one ~make_inp:make_inp_list rA rB label (g0 env))
end

module G = MakeGlobal(struct type 'a lin = 'a let mklin x = x let unlin x = x end)
include G
include L

let ipc cnt =
  EpIPCProcess
    (List.init cnt (fun _ ->
         Table.create (fun c ->
             List.init c (fun _ -> new_dpipe ()))))

let defaultlocal cnt =
  {multiplicity=cnt; epkind=EpLocal}

let defaultipc cnt =
  {multiplicity=cnt; epkind=ipc cnt}

let gen g =
  Global_common.gen_with_param
    {props=Table.create defaultlocal} g
  
let gen_ipc g =
  Global_common.gen_with_param
    {props=Table.create defaultipc} g

let gen_mult ps g =
  Global_common.gen_with_param
    {props=
       Table.create_with
         defaultlocal
         (List.map defaultlocal ps)}
    g

let gen_mult_ipc ps g =
  Global_common.gen_with_param
    {props=
       Table.create_with
         defaultipc
         (List.map defaultipc ps)}
    g

type kind = Local | IPCProcess
let epkind_of_kind = function
  | Local -> fun i -> {multiplicity=i; epkind=EpLocal}
  | IPCProcess -> fun i -> {multiplicity=i; epkind=ipc i}

let mkparams ps =
  {props =
     Table.create_with
       (fun _ -> failwith "gen: parameters not enough")
       (List.map (fun k -> epkind_of_kind k 1) ps)}

let mkparams_mult ps =
  {props =
     Table.create_with
       (fun _ -> failwith "gen: parameters not enough")
       (List.map (fun (k,p) -> epkind_of_kind k p) ps)}

let gen_with_kinds ps g =
  Global_common.gen_with_param
    (mkparams ps)
    g

let gen_with_kind_params ps g =
  Global_common.gen_with_param
    (mkparams_mult ps)
    g
let gen = gen_ipc
