open Base
open Common

module Make
         (EP:S.LIN_EP)
         (M:S.MONAD)
         (EV:S.EVENT with type 'a monad = 'a M.t)
         (C:S.SERIAL with type 'a monad = 'a M.t)
         (Lin:S.LIN)
  = struct

  include Global_common.Make(EP)

  module Out = Out.Make(EP)(EV)
  module Inp = Inp.Make(EP)(M)(EV)

  module Dpipe = Make_dpipe(C)

  type epkind =
    EpLocal
  | EpIPCProcess of Dpipe.dpipe list Table.t list
  | EpUntyped of (Base.tag * Obj.t) EV.channel list Table.t list

  open Out
  open Inp
  open Dpipe

  type 'v chan =
    | Bare of 'v EV.channel list ref
    | Untyped of tag * (tag * Obj.t) EV.channel list
    | IPC of tag * Dpipe.dpipe list

  let bare_of_chan = function
    | Bare xs ->
       BareOutChan xs
    | Untyped (tag, chs) ->
       let real_send ch v =
         EV.sync (EV.send ch (tag, (Obj.repr v)))
       in
       BareOutFun (List.map (fun ch -> real_send ch) chs)
    | IPC (tag, chs) ->
       let real_out out v =
         M.bind (C.output_tagged out (tag, (Obj.repr v))) (fun () ->
         C.flush out)
       in
       BareOutFun (List.map (fun {me={out;_};_} -> real_out out) chs)

  type 'g global = (epkind, 'g) t

  let make_inp_one chs myidx wrapfun =
    match List.hd chs with
    | Bare chs ->
       let resolve_ch () =
             (* we must delay this -- chs is a placeholder for channels
              * which might be "unified" during merge (see out.ml).
              * if we do not delay, and if the channel unification occurs,
              * input will block indefinitely.
              *)
             let ch = List.nth !chs myidx in
             EV.flip_channel ch
       in
       EP.make (fun once -> (* this lambda does NOT delay if linearity check is static *)
           InpChan (once,
                    EV.guard (fun () -> (* so, this guard is mandatory *)
                        EV.wrap (EV.receive (resolve_ch ())) wrapfun)))
    | Untyped (tag,chs) ->
       let ch = List.nth chs myidx in
       let ch = EV.flip_channel ch in
       EP.make (fun once ->
           InpFun (once, (fun () -> EV.sync (EV.receive ch)), [(tag, (fun t -> wrapfun (Obj.obj t)))]))
    | IPC (tag, chs) ->
       let {me={inp=ch;_};_} = flip_dpipe (List.nth chs myidx) in
       EP.make (fun once ->
           InpFun
             (once, (fun () -> C.input_tagged ch), [(tag, (fun t -> wrapfun (Obj.obj t)))]))

  let make_inp_list chs myidx wrapfun =
    match chs with
    | ((Bare _) :: _) ->
       let ext = function
         | Bare chs -> chs
         | _ -> assert false
       in
       let resolve_ch () =
         let chs = List.map ext chs in
         let chs = List.map (fun chs -> List.nth !chs(*delayed*) myidx) chs in
         List.map EV.flip_channel chs
       in
       EP.make (fun once -> InpChan (once, EV.guard (fun () -> EV.wrap (EV.receive_list (resolve_ch ())) wrapfun)))
    | ((Untyped (tag,_)) :: _) ->
       let ext = function
         | Untyped (_,chs) -> chs
         | _ -> assert false
       in
       let chs = List.map ext chs in
       let chs = List.map (fun chs -> EV.flip_channel (List.nth chs myidx)) chs in
       EP.make (fun once ->
           InpFun (once,
                   (fun () ->
                     M.bind (EV.sync @@ EV.receive_list chs) @@ fun vs ->
                     let tag = fst (List.hd vs) in
                     let vs = List.map snd vs in
                     M.return (tag, Obj.repr vs)),
                   [(tag, (fun t -> wrapfun (Obj.obj t)))]))
    | (IPC (tag,_) :: _) ->
       let ext = function
         | IPC (_,chs) -> List.nth chs myidx
         | _ -> assert false
       in
       let chs = List.map ext chs in
       let chs = List.map flip_dpipe chs in
       let chs = List.map (fun {me={inp;_};_} -> inp) chs in
       EP.make (fun once ->
           InpFun
             (once,
              (fun () ->
                M.bind (C.input_value_list chs) @@ fun vs ->
                let tag = fst (List.hd vs) in
                let vs = List.map snd vs in
                M.return (tag, Obj.repr vs)),
              [(tag, (fun t -> wrapfun (Obj.obj t)))]))
    | [] ->
       failwith "no channel"

  let make_recv ~make_inp num_receivers rA lab chs epB =
    if num_receivers=0 then begin
        failwith "make_recv: scatter/gather error: number of receivers is zero"
      end;
    if List.length chs = 0 then begin
        failwith "make_recv: scatter/gather error: number of senders is zero"
      end;
    let channels =
      List.init num_receivers (fun myidx ->
          let wrapfun v = lab.var (v, Lin.mklin (List.nth (Mergeable.out epB) myidx))
          in
          make_inp chs myidx wrapfun)
    in
    let hook =
      lazy begin
          (* force the following endpoints *)
          let eps = Mergeable.out epB in
          if num_receivers <> List.length eps then begin
              failwith "make_recv: endpoint count inconsistency"
            end
        end
    in
    Mergeable.wrap_obj rA.role_label
      (Mergeable.make_with_hook
         hook
         Inp.merge_in
         channels)

  let make_out_one (a,b,(c,d)) = Out.Out (a,b,(c,d))
  let make_out_list (a,b,(c,d)) = Out.OutMany (a,b,(c,d))

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
          EP.make (fun once -> Lin.mklin (make_out (once,ch,(k,epA)))))
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
            (fun o1 o2 -> Lin.mklin (Out.merge_out (Lin.unlin o1) (Lin.unlin o2)))
            epA'))

  let update_other_tables flip src_tables src_role other_tables other_role =
    let chss = List.map (fun t -> Table.get t other_role) src_tables in
    let chss = transpose chss in
    let flips ks = List.map flip ks in
    List.iter2
      (fun t ch -> Table.put t src_role (flips ch))
      other_tables
      chss

  let get_or_add_channels make flip src_tables src_role other_tables other_role other_count =
    let make () =
      List.init other_count (fun _ -> make ())
    in
    let chss =
      List.map
        (fun t -> Table.get_or_add t other_role make)
        src_tables
    in
    begin match other_tables with
    | Some other_tables ->
       update_other_tables flip src_tables src_role other_tables other_role;
    | None -> ()
    end;
    chss

  let ipc_table = function
    | EpIPCProcess ts -> Some ts
    | EpLocal -> None
    | EpUntyped _ -> None

  let untyped_table = function
    | EpUntyped ts -> Some ts
    | EpIPCProcess _ -> None
    | EpLocal -> None

  let generate_channels label arole brole akind bkind acount bcount =
    match akind, bkind with
    | EpLocal, EpLocal ->
       List.init acount (fun _ ->
           Bare(ref @@ List.init bcount (fun _ -> EV.new_channel ())))
    | EpIPCProcess kts, bkind ->
       assert (List.length kts = acount);
       let chss =
         get_or_add_channels new_dpipe flip_dpipe kts arole (ipc_table bkind) brole bcount
       in
       List.map (fun chs -> IPC(make_tag label.var, chs)) chss
    | akind, EpIPCProcess kts ->
       assert (List.length kts = bcount);
       let chss =
         get_or_add_channels new_dpipe flip_dpipe kts brole (ipc_table akind) arole acount
       in
       (* transpose and flip *)
       let chss = transpose chss in
       let chss = List.map (List.map flip_dpipe) chss in
       List.map (fun chs -> IPC(make_tag label.var, chs)) chss
    | EpUntyped kts, bkind ->
       assert (List.length kts = acount);
       let chss =
         get_or_add_channels EV.new_channel EV.flip_channel kts arole (untyped_table bkind) brole bcount
       in
       List.map (fun chs -> Untyped (make_tag label.var, chs)) chss
    | akind, EpUntyped kts ->
       assert (List.length kts = bcount);
       let chss =
         get_or_add_channels EV.new_channel EV.flip_channel kts brole (untyped_table akind) arole acount
       in
       (* transpose and flip *)
       let chss = transpose chss in
       let chss = List.map (List.map EV.flip_channel) chss in
       List.map (fun chs -> Untyped (make_tag label.var, chs)) chss

  let multiplicity {props;_} idx =
    option ~dflt:1 ~f:(fun x -> x.multiplicity) (Table.get_opt props idx)

  let epkind {props;default} idx cnt =
    match Table.get_opt props idx with
    | Some p -> p.epkind
    | None ->
       let kind = default cnt in
       let p = {multiplicity=cnt; epkind=kind} in
       Table.put props idx p;
       kind

  let a2b env ?num_senders ?num_receivers ~make_out ~make_inp = fun rA rB label g0 ->
    let acount = of_option num_senders ~dflt:(multiplicity env (Seq.int_of_lens rA.role_index))
    and bcount = of_option num_receivers ~dflt:(multiplicity env (Seq.int_of_lens rB.role_index))
    and arole = Seq.int_of_lens rA.role_index
    and brole = Seq.int_of_lens rB.role_index
    in
    let akind = epkind env arole acount
    and bkind = epkind env brole bcount
    in
    let chs = generate_channels label arole brole akind bkind acount bcount
    in
    let epB = Seq.get rB.role_index g0 in
    let ev  = make_recv bcount ~make_inp rA label chs epB in
    let g1  = Seq.put rB.role_index g0 ev
    in
    let epA = Seq.get rA.role_index g1 in
    let obj = make_send ~make_out acount rB label chs epA in
    let g2  = Seq.put rA.role_index g1 obj
    in g2

  let ( --> ) : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
                (< .. > as 'roleAobj, 'labelvar Inp.inp, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
                (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
                (< .. > as 'labelobj, [> ] as 'labelvar, ('v one * 'epA) Out.out Lin.lin, 'v * 'epB Lin.lin) label ->
                'g0 global -> 'g2 global
    = fun rA rB label (Global g0) ->
    Global (fun env ->
        a2b env ~num_senders:1 ~num_receivers:1 ~make_out:make_out_one ~make_inp:make_inp_one rA rB label (g0 env))

  let scatter : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
                (< .. > as 'roleAobj, 'labelvar Inp.inp, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
                (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
                (< .. > as 'labelobj, [> ] as 'labelvar, ('v list * 'epA) Out.out Lin.lin, 'v * 'epB Lin.lin) label ->
                'g0 global -> 'g2 global
    = fun rA rB label (Global g0) ->
    Global (fun env ->
        a2b env ~num_senders:1 ~make_out:make_out_list ~make_inp:make_inp_one rA rB label (g0 env))

  let gather : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
               (< .. > as 'roleAobj, 'labelvar Inp.inp, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
               (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
               (< .. > as 'labelobj, [> ] as 'labelvar, ('v one * 'epA) Out.out Lin.lin, 'v list * 'epB Lin.lin) label ->
               'g0 global -> 'g2 global
    = fun rA rB label (Global g0) ->
    Global (fun env ->
        a2b env ~num_receivers:1 ~make_out:make_out_one ~make_inp:make_inp_list rA rB label (g0 env))

  let local _ = EpLocal


  let ipc cnt =
    EpIPCProcess (List.init cnt (fun _ -> Table.create ()))

  let untyped cnt =
    EpUntyped (List.init cnt (fun _ -> Table.create ()))

  let gen g =
    gen_with_param
      {props=Table.create (); default=local} g

  let gen_ipc g =
    gen_with_param
      {props=Table.create (); default=ipc} g

  let gen_mult ps g =
    let ps = List.map (fun cnt -> {multiplicity=cnt;epkind=EpLocal}) ps in
    gen_with_param
      {props=Table.create_from ps; default=local}
      g

  let gen_mult_ipc ps g =
    let ps = List.map (fun cnt -> {multiplicity=cnt;epkind=ipc cnt}) ps in
    gen_with_param
      {props=Table.create_from ps; default=ipc}
      g

  type kind = [`Local | `IPCProcess | `Untyped]
  let epkind_of_kind = function
    | `Local -> fun i -> {multiplicity=i; epkind=EpLocal}
    | `IPCProcess -> fun i -> {multiplicity=i; epkind=ipc i}
    | `Untyped -> fun i -> {multiplicity=i; epkind=untyped i}

  let mkparams ps =
    {props =
       Table.create_from
         (List.map (fun k -> epkind_of_kind k 1) ps);
     default=local}

  let mkparams_mult ps =
    {props =
       Table.create_from
         (List.map (fun (k,p) -> epkind_of_kind k p) ps);
     default=local}

  let gen_with_kinds ps g =
    gen_with_param
      (mkparams ps)
      g

  let gen_with_kinds_mult ps g =
    gen_with_param
      (mkparams_mult ps)
      g

  type _ shared =
    Shared :
      {global: [`cons of 'ep * 'tl] global;
       kinds: kind list option;
       accept_lock: Mutex.t; (* FIXME: parameterise over other lock types? *)
       connect_sync: epkind env EV.channel list;
       start_sync: unit EV.channel;
       mutable seq_in_process: (epkind env * [`cons of 'ep * 'tl] Seq.t) option;
      } -> [`cons of 'ep * 'tl] shared


  let rec sync_all_ except_me connect_sync start_sync env =
    M.iteriM (fun i c ->
        if i=except_me then begin
          M.return_unit
          end else begin
          M.bind (EV.sync (EV.send c env)) (fun () ->
              EV.sync (EV.receive start_sync))
          end
      )
      connect_sync


  let init_seq_ (Shared m) =
    match m.seq_in_process with
    | Some (env,g) ->
       env,g
    | None ->
       let env =
         match m.kinds with
         | Some kinds -> mkparams kinds
         | None -> {props=Table.create (); default=local}
       in
       let g = gen_with_param env m.global in
       m.seq_in_process <- Some (env,g);
       env,g

  let create_shared ?kinds global =
    let accept_lock = Mutex.create () in
    let env =
      match kinds with
      | Some kinds -> mkparams kinds
      | None -> {props=Table.create (); default=local}
    in
    let seq = gen_with_param env global in
    let len = Seq.effective_length seq in
    let connect_sync = List.init len (fun _ -> EV.new_channel ()) in
    Shared
      {global;
       kinds;
       accept_lock;
       connect_sync;
       start_sync=EV.new_channel ();
       seq_in_process=Some (env,seq)}

  let accept_ (Shared m) r =
    Mutex.lock m.accept_lock;
    let env, g = init_seq_ (Shared m) in
    (* sync with all threads *)
    let me = Seq.int_of_lens r.role_index in
    M.bind (sync_all_ me m.connect_sync m.start_sync env) (fun () ->
    (* get my ep *)
    let ep = get_ep r g in
    m.seq_in_process <- None;
    Mutex.unlock m.accept_lock;
    let prop = Table.get env.props (Seq.int_of_lens r.role_index) in
    M.return (ep, prop))

  let connect_ (Shared m) r =
    let role = Seq.int_of_lens r.role_index in
    let c = EV.flip_channel (List.nth m.connect_sync role) in
    M.bind (EV.sync (EV.receive c)) (fun env ->
        let prop = Table.get env.props role in
        let g = match m.seq_in_process with Some (_,g) -> g | None -> assert false in
        let ep = get_ep r g in
        M.bind (EV.sync (EV.send (EV.flip_channel m.start_sync) ())) (fun () ->
        M.return (ep, prop)))

  let accept sh r =
    M.map fst (accept_ sh r)

  let connect sh r =
    M.map fst (connect_ sh r)

  let accept_and_start sh r f =
    M.bind (accept_ sh r) (fun (ep,prop) ->
        match prop.epkind with
        | EpLocal | EpUntyped _ ->
           ignore (Thread.create (fun () -> (f ep : unit)) ());
           M.return_unit
        | EpIPCProcess _ ->
           Common.fork_child (fun () -> f ep) ();
           M.return_unit)

  let connect_and_start sh r f =
    M.bind (connect_ sh r) (fun (ep,prop) ->
        match prop.epkind with
        | EpLocal | EpUntyped _ ->
           ignore (Thread.create (fun () -> (f ep : unit)) ());
           M.return_unit
        | EpIPCProcess _ ->
           Common.fork_child (fun () -> f ep) ();
           M.return_unit)

  let (>:) :
        ('obj,'var,('v Lin.lin one * 'epA) Out.out Lin.lin, 'v Lin.lin * 'epB Lin.lin) label ->
        (unit -> 'v) ->
        ('obj,'var,('v Lin.lin one * 'epA) Out.out Lin.lin, 'v Lin.lin * 'epB Lin.lin) label =
    fun l _ -> l

  let prot a g () = get_ep a (gen g)
end
