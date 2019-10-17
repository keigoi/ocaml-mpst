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

  module Out = Out.Make (EP)(M)(EV)
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

  type 'g global = (epkind, 'g) t

  let[@inline] make_inp_one chs label cont =
    let case tag myidx =
      [(tag, (fun t -> label.var (Obj.obj t, EP.fresh cont myidx)))]
    in
    match chs with
    | Bare(ch) :: _ ->
       let chs = List.map (function
                     | Bare(ch) -> ch
                     | _ -> assert false) chs
       in
       Inp.create_inp_one chs label cont
    | Untyped (tag,chs) :: _ ->
       let inpfun myidx ch =
         let ch = EV.flip_channel ch in
         {raw_input=(fun () -> EV.sync (EV.receive ch));
          cases=case tag myidx
         }
       in
       Inp.create_inp_fun (List.mapi inpfun chs)
    | IPC (tag, chs) :: _ ->
       let inpfun myidx ch =
         let ch = flip_dpipe ch in
         {raw_input=(fun () ->
            M.bind (C.input_tagged ch.me.inp) (fun v ->
              M.return v));
          cases=case tag myidx
         }
       in
       Inp.create_inp_fun (List.mapi inpfun chs)
    | [] ->
       assert false

  let[@inline] make_inp_many chs label cont =
    let case tag myidx =
      [(tag, (fun t -> label.var (Obj.obj t, EP.fresh cont myidx)))]
    in
    match chs with
    | Bare(_)::_ ->
       let chs = List.map (function
                     | Bare(ch) -> ch
                     | _ -> assert false) chs
       in
       Inp.create_inp_many chs label cont

    | Untyped (tag, _) ::_ ->
       let chss = List.map (function
                     | Untyped(_,ch) -> ch
                     | _ -> assert false) chs
       in
       let chss = List.map (List.map EV.flip_channel) chss in
       let inpfun myidx chs =
         let raw_input () =
           M.bind (EV.sync @@ EV.receive_list chs) (fun vs ->
               let tag = fst (List.hd vs) in
               let vs = List.map snd vs in
               M.return (tag, Obj.repr vs))
         in
         {raw_input;
          cases=case tag myidx}
       in
       Inp.create_inp_fun (List.mapi inpfun chss)

    | IPC (tag, _) :: _ ->
       let chss = List.map (function
                     | IPC(_,ch) -> ch
                     | _ -> assert false) chs
       in
       let chss = List.map (List.map flip_dpipe) chss in
       let chss = List.map (List.map (fun ch -> ch.me.inp)) chss in
       let inpfun myidx chs =
         let raw_input () =
           M.bind (C.input_value_list chs) (fun vs ->
           let tag = fst (List.hd vs) in
           let vs = List.map snd vs in
           M.return (tag, Obj.repr vs))
         in
         {raw_input;
          cases=case tag myidx}
       in
       Inp.create_inp_fun (List.mapi inpfun chss)
    | [] -> assert false

  let make_out_one chs label cont =
    match chs with
    | Bare(_)::_ ->
       let chs = List.map (function
                     | Bare(ch) -> ch
                     | _ -> assert false) chs
       in
       Out.create_out_one chs label cont
    | Untyped(tag,_)::_ ->
       let chs = List.map (function
                     | Untyped(_,ch) -> List.hd ch
                     | _ -> assert false) chs
       in
       let outfuns =
         List.map (fun ch v ->
             EV.sync (EV.send ch (tag, Obj.repr v))
           ) chs
       in
       Out.create_out_fun_one outfuns label cont
    | IPC(tag,_)::_ ->
       let chs = List.map (function
                     | IPC(_,ch) -> List.hd ch
                     | _ -> assert false) chs
       in
       let outfuns =
         List.map (fun ch v ->
             M.bind (C.output_tagged ch.me.out (tag, Obj.repr v)) (fun () ->
             C.flush ch.me.out)
           ) chs
       in
       Out.create_out_fun_one outfuns label cont
    | [] -> assert false

  let make_out_many chss label cont =
    match chss with
    | Bare(ch)::_ ->
       let chss = List.map (function
                      | Bare(chs) -> chs
                      | _ -> assert false) chss
       in
       Out.create_out_many chss label cont
    | Untyped(tag,_)::_ ->
       let chss = List.map (function
                      | Untyped(_,ch) -> ch
                      | _ -> assert false) chss
       in
       let real_out ch v =
         EV.sync (EV.send ch (tag, Obj.repr v))
       in
       Out.create_out_fun_many (List.map (List.map real_out) chss) label cont
    | IPC(tag,_)::_ ->
       let chss = List.map (function
                     | IPC(_,ch) -> ch
                     | _ -> assert false) chss
       in
       let real_out ch v =
         C.output_tagged ch.me.out (tag, Obj.repr v)
       in
       Out.create_out_fun_many (List.map (List.map real_out) chss) label cont
    | [] -> assert false


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
    let epB = Seq.lens_get rB.role_index g0 in
    let label0 = {label with var =(fun[@inline] (v,t) -> label.var (v, StaticLin.create_dummy t))} in
    let ev = make_inp chs label0 epB in
    let epB = EP.wrap_label rA.role_label ev in
    let g1  = Seq.lens_put rB.role_index g0 epB
    in
    let epA = Seq.lens_get rA.role_index g1 in
    let out = make_out chs label epA in
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
        a2b env ~num_senders:1 ~num_receivers:1 ~make_out:make_out_one ~make_inp:make_inp_one rA rB label (g0 env))

  let scatter : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
                (< .. > as 'roleAobj, 'labelvar Inp.inp EP.lin, 'epA, 'roleBobj, 'g1, 'g2) role ->
                (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0, 'g1) role ->
                (< .. > as 'labelobj, [> ] as 'labelvar, ('v list * 'epA) Out.out EP.lin, 'v * 'epB StaticLin.lin) label ->
                'g0 global -> 'g2 global
    = fun rA rB label (Global g0) ->
    Global (fun env ->
        a2b env ~num_senders:1 ~make_out:make_out_many ~make_inp:make_inp_one rA rB label (g0 env))

  let gather : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
               (< .. > as 'roleAobj, 'labelvar Inp.inp EP.lin, 'epA, 'roleBobj, 'g1, 'g2) role ->
               (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0, 'g1) role ->
               (< .. > as 'labelobj, [> ] as 'labelvar, ('v one * 'epA) Out.out EP.lin, 'v list * 'epB StaticLin.lin) label ->
               'g0 global -> 'g2 global
    = fun rA rB label (Global g0) ->
    Global (fun env ->
        a2b env ~num_receivers:1 ~make_out:make_out_one ~make_inp:make_inp_many rA rB label (g0 env))

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
    let ep = get_ch r g in
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
        let ep = get_ch r g in
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
           ignore (C.fork_child (fun () -> f ep));
           M.return_unit)

  let connect_and_start sh r f =
    M.bind (connect_ sh r) (fun (ep,prop) ->
        match prop.epkind with
        | EpLocal | EpUntyped _ ->
           ignore (Thread.create (fun () -> (f ep : unit)) ());
           M.return_unit
        | EpIPCProcess _ ->
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
