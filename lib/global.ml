open Base
open Common

module Make
         (EP:S.ENDPOINTS)
         (StaticLin:S.LIN)
         (M:S.MONAD)
         (EV:S.EVENT with type 'a monad = 'a M.t)
         (C:S.SERIAL with type 'a monad = 'a M.t)
  = struct

  include Global_common.Make(EP)
  include Channel.Make(EP)(StaticLin)(M)(EV)(C)

  type 'g global = (epkind, 'g) t
  type close = Close.close

  let rm_size {metainfo;_} idx =
    option ~dflt:1 ~f:(fun x -> x.rm_size) (Table.get_opt metainfo idx)

  let rm_kind {metainfo;default} idx cnt =
    match Table.get_opt metainfo idx with
    | Some p -> p.rm_kind
    | None ->
       let kind = default cnt in
       let p = {rm_index=idx; rm_size=cnt; rm_kind=kind} in
       Table.put metainfo idx p;
       kind

  let make_metainfo ?size env role =
    let rm_index = Seq.int_of_lens role.role_index in
    let rm_size = of_option size ~dflt:(rm_size env rm_index) in
    let rm_kind = rm_kind env rm_index rm_size in
    {rm_index; rm_kind; rm_size}


  let mkclose env role =
    let num =
      match Table.get_opt env.metainfo role with
      | Some prop -> prop.rm_size
      | None -> 1
    in
    EP.make_simple
      (List.init num (fun i ->
           Close.make_close (fun () ->
               match Table.get_opt env.metainfo role with
               | Some {rm_kind=EpDpipe table;_} ->
                  let chss = Table.to_list (List.nth table i) in
                  let chss = List.concat chss in
                  M.iteriM (fun _ c -> Dpipe.close_dpipe c) chss
               | _ -> M.return_unit)))

    
  let finish : (epkind, [`cons of close * 'a] as 'a) t =
    Global (fun env ->
        Seq.repeat 0 (mkclose env))

  let closed : 'g. (_, _, close, close, 'g, 'g) role -> (epkind,'g) t -> (epkind,'g) t
    = fun r (Global g) ->
    Global (fun env ->
        let g = g env in
        let close = mkclose env (Seq.int_of_lens r.role_index) in
        let g' = Seq.lens_put r.role_index g close in
        g')

  let a2b env ?num_senders ?num_receivers ~gen ~make_out = fun rA rB label g0 ->
    let from_info = make_metainfo ?size:num_senders env rA in
    let to_info = make_metainfo ?size:num_receivers env rB in
    let epB = Seq.lens_get rB.role_index g0 in
    let och,ich = gen label epB from_info to_info in
    let epB = EP.wrap_label rA.role_label ich in
    let g1  = Seq.lens_put rB.role_index g0 epB
    in
    let epA = Seq.lens_get rA.role_index g1 in
    let out = make_out label och epA in
    let epA = EP.wrap_label rB.role_label out in
    let g2  = Seq.lens_put rA.role_index g1 epA
    in g2

  let ( --> ) : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
                (< .. > as 'roleAobj, 'labelvar Inp.inp EP.lin, 'epA, 'roleBobj, 'g1, 'g2) role ->
                (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0, 'g1) role ->
                (< .. > as 'labelobj, [> ] as 'labelvar, ('v one * 'epA) Out.out EP.lin, 'v * 'epB StaticLin.lin) label ->
                'g0 global -> 'g2 global
    = fun rA rB label (Global g0) ->
    Global (fun env ->
        a2b ~gen:generate_one ~make_out:Out.make_out_single
          env rA rB label (g0 env)
      )

  let scatter : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
                (< .. > as 'roleAobj, 'labelvar Inp.inp EP.lin, 'epA, 'roleBobj, 'g1, 'g2) role ->
                (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0, 'g1) role ->
                (< .. > as 'labelobj, [> ] as 'labelvar, ('v list * 'epA) Out.out EP.lin, 'v * 'epB StaticLin.lin) label ->
                'g0 global -> 'g2 global
    = fun rA rB label (Global g0) ->
    Global (fun env ->
        a2b ~num_senders:1 ~gen:generate_scatter ~make_out:Out.make_outmany
          env rA rB label (g0 env)
      )

  let gather : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
               (< .. > as 'roleAobj, 'labelvar Inp.inp EP.lin, 'epA, 'roleBobj, 'g1, 'g2) role ->
               (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0, 'g1) role ->
               (< .. > as 'labelobj, [> ] as 'labelvar, ('v one * 'epA) Out.out EP.lin, 'v list * 'epB StaticLin.lin) label ->
               'g0 global -> 'g2 global
    = fun rA rB label (Global g0) ->
    Global (fun env ->
        a2b ~num_receivers:1 ~gen:generate_gather ~make_out:Out.make_out
          env rA rB label (g0 env)
      )

  let local _ = EpLocal


  let ipc cnt =
    EpDpipe (List.init cnt (fun _ -> Table.create ()))

  let untyped cnt =
    EpUntyped (List.init cnt (fun _ -> Table.create ()))

  let gen g =
    gen_with_param
      {metainfo=Table.create (); default=local} g

  let gen_ipc g =
    gen_with_param
      {metainfo=Table.create (); default=ipc} g

  let gen_mult ps g =
    let ps = List.mapi (fun i cnt -> {rm_index=i;rm_size=cnt;rm_kind=EpLocal}) ps in
    gen_with_param
      {metainfo=Table.create_from ps; default=local}
      g

  let gen_mult_ipc ps g =
    let ps = List.mapi (fun i cnt -> {rm_index=i;rm_size=cnt;rm_kind=ipc cnt}) ps in
    gen_with_param
      {metainfo=Table.create_from ps; default=ipc}
      g

  type kind = [`Local | `IPCProcess | `Untyped]
  let rm_kind_of_kind ~rm_index ~rm_size = function
    | `Local -> {rm_kind=EpLocal; rm_size; rm_index}
    | `IPCProcess -> {rm_kind=ipc rm_size; rm_size; rm_index}
    | `Untyped -> {rm_kind=untyped rm_size; rm_size; rm_index}

  let mkparams ps =
    {metainfo =
       Table.create_from
         (List.mapi (fun rm_index k -> rm_kind_of_kind ~rm_index ~rm_size:1 k) ps);
     default=local}

  let mkparams_mult ps =
    {metainfo =
       Table.create_from
         (List.mapi (fun rm_index (k,rm_size) -> rm_kind_of_kind ~rm_index ~rm_size k) ps);
     default=local}

  let gen_with_kinds ps g =
    gen_with_param
      (mkparams ps)
      g

  let gen_with_kinds_mult ps g =
    gen_with_param
      (mkparams_mult ps)
      g

type 'a ty = Ty__ of (unit -> 'a StaticLin.lin)

let get_ty_ : ('x0, 'x1, 'ep StaticLin.lin, 'x2, 't, 'x3) role -> 't Seq.t -> 'ep ty =
  fun r g ->
  Ty__ (fun () -> get_ch r g)

let get_ty : ('x0, 'x1, 'ep StaticLin.lin, 'x2, 't, 'x3) role -> 't global -> 'ep ty =
  fun r g ->
  get_ty_ r (gen g)

  type _ shared =
    Shared :
      {global: [`cons of 'ep * 'tl] global;
       kinds: kind list option;
       accept_lock: M.mutex;
       connect_sync: (epkind env * [`cons of 'ep * 'tl] Seq.t) EV.channel list;
       start_sync: unit EV.channel;
      } -> [`cons of 'ep * 'tl] shared


  let rec sync_all_ myroleid connect_sync start_sync env =
    M.iteriM (fun i c ->
        if i=myroleid then begin
            M.return_unit
          end else begin
            M.bind (EV.sync (EV.send c env)) (fun () ->
              EV.sync (EV.receive start_sync))
          end
      )
      connect_sync


  let init_seq_ (Shared m) =
       let env =
         match m.kinds with
         | Some kinds -> mkparams kinds
         | None -> {metainfo=Table.create (); default=local}
       in
       let g = gen_with_param env m.global in
       env,g

  let create_shared ?kinds global =
    let accept_lock = M.create_mutex () in
    let env =
      match kinds with
      | Some kinds -> mkparams kinds
      | None -> {metainfo=Table.create (); default=local}
    in
    let seq = gen_with_param env global in
    let len = Seq.effective_length seq in
    let connect_sync = List.init len (fun _ -> EV.new_channel ()) in
    Shared
      {global;
       kinds;
       accept_lock;
       connect_sync;
       start_sync=EV.new_channel ();}

  let accept_ (Shared m) r =
    M.bind (M.lock m.accept_lock) (fun () ->
    let env, g = init_seq_ (Shared m) in
    (* sync with all threads *)
    let me = Seq.int_of_lens r.role_index in
    M.bind (sync_all_ me m.connect_sync m.start_sync (env,g)) (fun () ->
    (* get my ep *)
    let ep = get_ch r g in
    M.unlock m.accept_lock;
    let prop = Table.get env.metainfo (Seq.int_of_lens r.role_index) in
    M.return (ep, prop)))

  let connect_ (Shared m) r =
    let role = Seq.int_of_lens r.role_index in
    let c = EV.flip_channel (List.nth m.connect_sync role) in
    M.bind (EV.sync (EV.receive c)) (fun (env,g) ->
        let prop = Table.get env.metainfo role in
        let ep = get_ch r g in
        M.bind (EV.sync (EV.send (EV.flip_channel m.start_sync) ())) (fun () ->
        M.return (ep, prop)))

  let accept sh r =
    M.map fst (accept_ sh r)

  let connect sh r =
    M.map fst (connect_ sh r)

  let accept_and_start sh r f =
    M.bind (accept_ sh r) (fun (ep,prop) ->
        match prop.rm_kind with
        | EpLocal | EpUntyped _ ->
           ignore (Thread.create (fun () -> (f ep : unit)) ());
           M.return_unit
        | EpDpipe _ ->
           ignore (C.fork_child (fun () -> f ep));
           M.return_unit)

  let connect_and_start sh r f =
    M.bind (connect_ sh r) (fun (ep,prop) ->
        match prop.rm_kind with
        | EpLocal | EpUntyped _ ->
           ignore (Thread.create (fun () -> (f ep : unit)) ());
           M.return_unit
        | EpDpipe _ ->
           ignore (C.fork_child (fun () -> f ep));
           M.return_unit)

  let (>:) :
        ('obj,'var,('v StaticLin.lin one * 'epA) Out.out EP.lin, 'v StaticLin.lin * 'epB StaticLin.lin) label ->
        (unit -> 'v) ->
        ('obj,'var,('v StaticLin.lin one * 'epA) Out.out EP.lin, 'v StaticLin.lin * 'epB StaticLin.lin) label =
    fun l _ -> l

  let (>>:) :
        ('obj,'var,('v EP.lin list * 'epA) Out.out EP.lin, 'v EP.lin * 'epB StaticLin.lin) label ->
        (unit -> 'v) ->
        ('obj,'var,('v EP.lin list * 'epA) Out.out EP.lin, 'v EP.lin * 'epB StaticLin.lin) label =
    fun l _ -> l

  let prot a g () = get_ch a (gen g)
end[@@inline]
