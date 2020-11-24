open Concur_shims

module type LIN = sig
  type +'a lin
  val mklin : 'a -> 'a lin
  val unlin_ : 'a lin -> 'a 
end

module Make(DynLin:Dyn_lin.S)(Lin:LIN) 
: sig
  include S.COMM

  include S.GLOBAL_COMBINATORS 
    with type 'a lin = 'a Lin.lin
    and type ('v,'t) out := ('v,'t) out
    and type 'var inp := 'var inp
    and type ('v,'t) out_many := ('v,'t) out_many
    and type 'var inp_many := 'var inp_many
    and type close := close

  include S.GEN
    with type 't global := 't global
    and type 'a lin := 'a lin
    and type 'a ty := 'a ty
    and type close := close
end
= struct

  module Low = Low_channel.Make(struct
                       type 'a t = 'a DynLin.gen
                       and 'a u = 'a Lin.lin
                       let unfresh_ x = DynLin.declare_unlimited @@ Lin.unlin_ x
                       let fresh x = Lin.mklin @@ DynLin.fresh x
                     end)

  module Single = Channel_vectors.MakeSingle(Low)(DynLin)

  module ScatterGather = Channel_vectors.MakeScatterGather(Low)(DynLin)

  module Seq = Seq.Make(struct type 'a t = 'a DynLin.gen end)
  
  type 't global = Env.t -> 't Seq.t

  type 'a lin = 'a Lin.lin

  type close_ = Single.close

  type ('obj,'ot,'var,'vt) label = ('obj,'ot,'var,'vt) Types.label 

  type ('ts, 't, 'us, 'u, 'robj, 'mt) role = ('ts, 't, 'us, 'u, 'robj, 'mt) Types.role

  type ('lr, 'l, 'r) disj = ('lr, 'l, 'r) Types.disj

  type 'a one = 'a Types.one

  exception InvalidEndpoint = Dyn_lin.InvalidEndpoint

  open Types
  open Base

  let is_typed env from to_ =
    let from = int_of_idx from.role_index
    and to_ = int_of_idx to_.role_index in
    let from_info = Env.metainfo env from 1
    and to_info = Env.metainfo env to_ 1
    in
    from_info.rm_kind = Env.EpLocal
    && to_info.rm_kind = Env.EpLocal

  (**
   * Communication combinator
   *)
  let (-->) ri rj label (g0 : _ global) : _ global = fun env ->
    let g0 = g0 env in
    let sj' = Seq.get rj.role_index g0 in
    let out, inp =
      if is_typed env ri rj then
        Low.create label.var sj'
      else
        Low.create_untyped env ~from:(int_of_idx ri.role_index) ~to_:(int_of_idx rj.role_index) label sj'
    in
    let sj = Single.declare_inp ri.role_label inp sj' in
    let g1 = Seq.put rj.role_index g0 sj in
    let si' = Seq.get ri.role_index g1 in
    let si = Single.declare_out rj.role_label label.obj out si' in
    let g2 = Seq.put ri.role_index g1 si in
    g2

  let gather ri rj label (g0 : _ global) : _ global = fun env ->
    let count = Env.rm_size env (int_of_idx ri.role_index) in
    let g0 = g0 env in
    let sj' = Seq.get rj.role_index g0 in
    let outs, inpmany =
    if is_typed env ri rj then
      Low.create_inp_many count label.var sj'
    else
      Low.create_untyped_inp_many env ~from:(int_of_idx ri.role_index) ~to_:(int_of_idx rj.role_index) label sj'
    in
    let sj = ScatterGather.declare_inp_many ri.role_label inpmany sj' in
    let g1 = Seq.put rj.role_index g0 sj in
    let si' = Seq.get_list ~size:count ri.role_index g1 in
    let si = List.map2 (Single.declare_out rj.role_label label.obj) outs si' in
    let g2 = Seq.put_list ri.role_index g1 si in
    g2

  let scatter ri rj label (g0 : _ global) : _ global = fun env ->
    let count = Env.rm_size env (int_of_idx rj.role_index) in
    let g0 = g0 env in
    let sj' = Seq.get_list rj.role_index ~size:count g0 in
    let out_many, inps =
      if is_typed env ri rj then
        Low.create_out_many label.var sj'
      else
        Low.create_untyped_out_many env ~from:(int_of_idx ri.role_index) ~to_:(int_of_idx rj.role_index) label sj'
    in
    let sj = List.map2 (Single.declare_inp ri.role_label) inps sj' in
    let g1 = Seq.put_list rj.role_index g0 sj in
    let si' = Seq.get ri.role_index g1 in
    let si = ScatterGather.declare_out_many rj.role_label label.obj out_many si' in
    let g2 = Seq.put ri.role_index g1 si in
    g2

  let munit =
    let unit = DynLin.declare_unlimited () in
    Mergeable.make ~value:unit ~mergefun:(fun _ _ -> unit) ()

  let choice_at
    = fun r disj (r',g0left) (r'',g0right) env ->
        let g0left, g0right = g0left env, g0right env in
        let epL, epR =
          Seq.get r'.role_index g0left,
          Seq.get r''.role_index g0right in
        let g1left, g1right =
          Seq.put r'.role_index g0left munit,
          Seq.put r''.role_index g0right munit in
        let g1 = Seq.seq_merge g1left g1right in
        let disj = DynLin.lift_disj disj in (* lift *)
        let ep = Mergeable.make_disj disj epL epR
        in
        let g2 = Seq.put r.role_index g1 ep
        in
        g2

  module Fix = struct
    type (_,_) idxs =
        | [] : ('aa, 'aa) idxs
        | (::) : ('a,'b,'aa,'bb,_,_) role * ('bb, 'cc) idxs -> ('aa, 'cc) idxs

    let fix : type g. (g,[`cons of close_ one * 'a] as 'a) idxs -> (g global -> g global) -> g global = fun _idxs f env ->
      let rec body =
        lazy ((f ((fun _ -> Seq.recvar body))) env)
      in
      (* A "fail-fast" approach to detect unguarded loops.
      * Seq.partial_force tries to fully evaluate unguarded recursion variables
      * in the body.
      *)
      Seq.resolve_merge (Lazy.force body)
  end

  let fix : type g. (g global -> g global) -> g global = fun f env ->
    let rec body =
      lazy ((f ((fun _ -> Seq.recvar body))) env)
    in
    (* A "fail-fast" approach to detect unguarded loops.
     * Seq.partial_force tries to fully evaluate unguarded recursion variables
     * in the body.
    *)
    Seq.resolve_merge (Lazy.force body)

  let make_close env role i =
    let info = Env.metainfo env role 1  in
    match info.rm_kind with
    | EpLocal | EpUntyped _ ->
      (* do nothing *)
      Single.declare_close IO.return
    | Env.EpDpipe table ->
      (* close pipe *)
      Single.declare_close begin fun () ->
        let chss = Table.to_list (List.nth table i) in
        let chss = List.concat chss in
        IO_list.iteri (fun _ c -> Untyped_dpipe.close_dpipe c) chss
      end

  let finish_seq env =
    Seq.repeat 0 (fun role -> make_close env role 0)

  let finish : ([`cons of close_ one * 'a] as 'a) global =
    finish_seq

  let finish_with_multirole :
      at:(close_ one, close_ list, [ `cons of close_ one * 'a ] as 'a, 'g, _, _) role ->
      'g global = fun ~at env ->
    let role = int_of_idx at.role_index in
    let count = Env.rm_size env role in
    let seq = finish_seq env in
    let g' = Seq.put_list at.role_index seq (List.init count (fun i -> make_close env role i)) in
    g'

  let closed_at : 'g. (close_ one, close_ one, 'g, 'g, _, _) role -> 'g global -> 'g global
    = fun r g env ->
      let g = g env in
      let role = int_of_idx r.role_index in
      let g' = Seq.put r.role_index g (make_close env role 0) in
      g'

  let closed_list_at_ r g env =
      let g = g env in
      let count = Env.rm_size env (int_of_idx r.role_index) in
      let role = int_of_idx r.role_index in
      let g' = Seq.put_list r.role_index g (List.init count (fun i -> make_close env role i)) in
      g'

  let closed_list_at : 'g. (close_ list, close_ list, 'g, 'g, _, _) role -> 'g global -> 'g global
    = closed_list_at_

  let with_multirole ~at = closed_list_at_ at

  type 't tup = Env.t * 't Seq.t

  let ipc cnt =
    Env.EpDpipe (List.init cnt (fun _ -> Table.create ()))

  let untyped cnt =
    Env.EpUntyped (List.init cnt (fun _ -> Table.create ()))

  let rm_kind_of_kind ~rm_index ~rm_size = function
    | `Local -> {Env.rm_kind=EpLocal; rm_size; rm_index}
    | `IPCProcess -> {Env.rm_kind=ipc rm_size; rm_size; rm_index}
    | `Untyped -> {Env.rm_kind=untyped rm_size; rm_size; rm_index}

  let gen_with_env env g = (env, g env)

  let gen g =
    gen_with_env
      {Env.metainfo=Table.create (); default=(fun _ -> EpLocal)}
      g

  (* let gen_ipc g =
   *   gen_with_env
   *     {Env.metainfo=Table.create (); default=ipc} g *)

  let local _ = Env.EpLocal

  let gen_mult ps g =
    let ps = List.mapi (fun i cnt -> {Env.rm_index=i;rm_size=cnt;rm_kind=EpLocal}) ps in
    gen_with_env
      {Env.metainfo=Table.create_from ps; default=local}
      g

  let mkparams_mult ps =
    {Env.metainfo =
       Table.create_from
         (List.mapi (fun rm_index (k,rm_size) -> rm_kind_of_kind ~rm_index ~rm_size k) ps);
     default=local}

  let mkparams ps =
    {Env.metainfo =
       Table.create_from
         (List.mapi (fun rm_index k -> rm_kind_of_kind ~rm_index ~rm_size:1 k) ps);
     default=local}

  let gen_with_kinds ps g =
    let env = mkparams ps in
    env, g env

  let gen_with_kinds_mult ps g =
    let env = mkparams_mult ps in
    env, g env

  (* let gen_mult_ipc ps g =
   *   let ps = List.mapi (fun i cnt -> {Env.rm_index=i;rm_size=cnt;rm_kind=ipc cnt}) ps in
   *   gen_with_env
   *     {Env.metainfo=Table.create_from ps; default=ipc}
   *     g *)

  let effective_length (_, s) =
    Seq.effective_length s

  let get_ch role (_, seq) =
    DynLin.fresh @@ Mergeable.resolve @@ Seq.get role.role_index seq

  let get_ch_list role (env, seq) =
    let size = Env.rm_size env @@ int_of_idx role.role_index in
    List.map (fun x -> DynLin.fresh @@ Mergeable.resolve x)
      @@ Seq.get_list ~size role.role_index seq

  let get_ch_ role ((env, seq) as tup) =
    let ch = get_ch role tup in
    let seq' = Seq.put role.role_index seq (Single.declare_close IO.return) in
    ch, (env, seq')

  let get_ch_list_ role ((env, seq) as tup) =
    let ch = get_ch_list role tup in
    let seq' = Seq.put role.role_index seq (Single.declare_close IO.return) in
    ch, (env, seq')

  type 'a ty =
      Ty__ of (unit -> 'a)
    | TyList__ of (unit -> 'a list)

  let get_ty_ = fun r g ->
    Ty__ (fun () -> Lin.mklin @@ get_ch r g)

  let get_ty = fun r g ->
    get_ty_ r (gen g)

  let get_ty_list_ = fun r g ->
    TyList__ (fun () -> List.map Lin.mklin @@ get_ch_list r g)

  let get_ty_list = fun r g ->
    get_ty_list_ r (gen g)

  let (>:) l _ = l

  let env (env, _) = env

  include Single
  include ScatterGather
end[@@inline]
