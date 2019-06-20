open Base
open Local

include Global_common

module Make
         (M:S.MONAD)
         (E:S.EVENT with type 'a monad = 'a M.t)
         (C:S.SERIAL with type 'a monad = 'a M.t)(Lin:S.LIN)
  = struct

  type pipe = {inp: C.in_channel; out: C.out_channel}
  type dpipe = {me:pipe; othr:pipe}

  let new_dpipe () =
    let my_inp, otr_out = C.pipe () in
    let otr_inp, my_out = C.pipe () in
    {me={inp=my_inp; out=my_out};
     othr={inp=otr_inp; out=otr_out}}

  let swap_dpipe {me=othr;othr=me} =
    {me;othr}

  module LocalChan = Local.Make(M)(E)
  open LocalChan

  type 'v chan =
    | Bare of 'v E.channel list ref
    | IPC of tag * dpipe list

  let bare_of_chan = function
    | Bare xs ->
       BareOutChan xs
    | IPC (tag, chs) ->
       let real_out out v =
         M.bind (C.output_tag out tag) (fun () ->
         M.bind (C.output_value out v) (fun () ->
         C.flush out))
       in
       BareOutIPC (List.map (fun {me={out;_};_} -> real_out out) chs)

  type 'g global = (dpipe, 'g) Global_common.t

  let make_inp_one chs myidx wrapfun =
    match List.hd chs with
    | Bare chs ->
       fun once ->
       (* we must delay this -- chs is a placeholder for channels which will change during merge *)
       let ch = List.nth !chs myidx in
       InpChan (once, E.wrap (E.receive ch) wrapfun)
    | IPC (tag, chs) ->
       let {me={inp=ch;_};_} = swap_dpipe (List.nth chs myidx) in
       fun once ->
       InpIPC
         (once, (fun () -> M.map (fun v -> [v]) (C.input_tag ch)),
          [(tag, (fun () -> M.map wrapfun (C.input_value ch)))])

  let make_inp_list chs myidx wrapfun =
    match chs with
    | ((Bare _) :: _) ->
       let ext = function
         | Bare chs -> chs
         | _ -> assert false
       in
       let chs = List.map ext chs in
       let ev =
         E.guard (fun () ->
             let chs = List.map (fun chs -> List.nth !chs(*delayed*) myidx) chs in 
             E.receive_list chs)
       in
       fun once ->
       InpChan (once, E.wrap ev wrapfun)
    | (IPC (tag,_) :: _) ->
       let ext = function
         | IPC (_,chs) -> List.nth chs myidx
         | _ -> assert false
       in
       let chs = List.map ext chs in
       let chs = List.map swap_dpipe chs in
       let chs = List.map (fun {me={inp;_};_} -> inp) chs in
       fun once ->
       InpIPC
         (once, (fun () -> C.input_value_list chs),
          [(tag, (fun () -> M.map wrapfun (C.input_value_list chs)))])
    | [] ->
       failwith "no channel"

  type inpkind = InpOne | InpList

  let make_recv ~make_inp num_receivers rA lab chs epB =
    if num_receivers=0 then begin
        failwith "make_recv: scatter/gather error: number of receivers is zero"
      end;
    if List.length chs = 0 then begin
        failwith "make_recv: scatter/gather error: number of senders is zero"
      end;
    let bare_inp_chs =
      List.init num_receivers (fun myidx ->
          let wrapfun v = lab.var (v, Lin.mklin (List.nth (Mergeable.out epB) myidx))
          in
          make_inp chs myidx wrapfun)
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
         LocalChan.merge_in
         bare_inp_chs)

  let make_out_one (a,b,(c,d)) = LocalChan.Out (a,b,(c,d))
  let make_out_list (a,b,(c,d)) = LocalChan.OutMany (a,b,(c,d))

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
          fun once -> Lin.mklin (make_out (once,ch,(k,epA))))
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
            (fun o1 o2 -> Lin.mklin (LocalChan.merge_out (Lin.unlin o1) (Lin.unlin o2)))
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
             Bare(ref @@ List.init bnum (fun _ -> E.new_channel ())))
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
                (< .. > as 'roleAobj, 'labelvar LocalChan.inp, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
                (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
                (< .. > as 'labelobj, [> ] as 'labelvar, ('v one * 'epA) LocalChan.out Lin.lin, 'v * 'epB Lin.lin) label ->
                'g0 global -> 'g2 global
    = fun rA rB label (Seq g0) ->
    Seq (fun env ->
        a2b env ~num_senders:1 ~num_receivers:1 ~make_out:make_out_one ~make_inp:make_inp_one rA rB label (g0 env))

  let scatter : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
                (< .. > as 'roleAobj, 'labelvar LocalChan.inp, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
                (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
                (< .. > as 'labelobj, [> ] as 'labelvar, ('v list * 'epA) LocalChan.out Lin.lin, 'v * 'epB Lin.lin) label ->
                'g0 global -> 'g2 global
    = fun rA rB label (Seq g0) ->
    Seq (fun env ->
        a2b env ~num_senders:1 ~make_out:make_out_list ~make_inp:make_inp_one rA rB label (g0 env))

  let gather : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
               (< .. > as 'roleAobj, 'labelvar LocalChan.inp, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
               (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
               (< .. > as 'labelobj, [> ] as 'labelvar, ('v one * 'epA) LocalChan.out Lin.lin, 'v list * 'epB Lin.lin) label ->
               'g0 global -> 'g2 global
    = fun rA rB label (Seq g0) ->
    Seq (fun env ->
        a2b env ~num_receivers:1 ~make_out:make_out_one ~make_inp:make_inp_list rA rB label (g0 env))
end

module Pure = struct
  type 'a t = 'a
  let return a = a
  let return_unit = ()
  let bind x f = f x
  let map f x = f x
  let iteriM = List.iteri
  let mapM = List.map
end
module Event = struct
  include Event
  type 'a monad = 'a
  (* XXX a dumb implementation of receiving from multiple channels  *)
  let receive_list = function
    | [] ->
       Event.always []
    | ch::chs ->
       Event.wrap (Event.receive ch)
         (fun v ->
           v :: List.map (fun ch -> Event.sync @@ Event.receive ch) chs)
end
module Serial = struct
  type 'a monad = 'a
  type in_channel = Stdlib.in_channel
  type out_channel = Stdlib.out_channel
  let pipe () =
    let inp,out = Unix.pipe () in
    Unix.in_channel_of_descr inp, Unix.out_channel_of_descr out
  let input_value ch =
    Stdlib.input_value ch
  let input_tag =
    input_value
  let output_value =
    Stdlib.output_value
  let output_tag =
    output_value
  let flush ch =
    Stdlib.flush ch
  let input_value_list chs =
    let rec loop = function
      | [] -> []
      | ch::chs -> Stdlib.input_value ch::loop chs
    in
    loop chs
end

module G = Make(Pure)(Event)(Serial)(struct type 'a lin = 'a let mklin x = x let unlin x = x end)
module L = Local.Make(Pure)(Event)
include G
include L

module LwtSerial = struct
  type 'a monad = 'a Lwt.t
  type in_channel = Lwt_io.input_channel
  type out_channel = Lwt_io.output_channel
  let pipe () =
    let inp,out = Lwt_unix.pipe () in
    Lwt_io.of_fd Lwt_io.input inp, Lwt_io.of_fd Lwt_io.output out
  let input_value ch =
    Lwt_io.read_value ch
  let input_tag =
    input_value
  let output_value ch v =
    Lwt_io.write_value ch v
  let output_tag =
    output_value
  let flush =
    Lwt_io.flush
  let input_value_list chs =
    let rec loop acc = function
      | [] -> Lwt.return (List.rev acc)
      | ch::chs ->
         Lwt.bind (Lwt_io.read_value ch) (fun v ->
             loop (v::acc) chs)
    in loop [] chs
end
module Lwt = struct
  include Lwt
  let mapM = Lwt_list.map_p
  let iteriM = Lwt_list.iteri_p
end
module GLwt = Make(Lwt)(Base_comm.LwtEvent)(LwtSerial)(struct type 'a lin = 'a let mklin x = x let unlin x = x end)
module LLwt = Local.Make(Lwt)(Base_comm.LwtEvent)

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
