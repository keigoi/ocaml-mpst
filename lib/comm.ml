open Base
open Concur_shims

module type LIN = sig
  type +'a lin
  val mklin : 'a -> 'a lin
end

module NoStatic : LIN with type 'a lin = 'a = struct
  type 'a lin = 'a
  let mklin x = x
end

module Make(DynLin:DynLin.S)(Lin:LIN) : sig
  type 'a lin = 'a Lin.lin

  type ('v, 's) out
  type 'var inp
  type close
  type ('v, 's) scatter
  type 'var gather
  
  val send : ('v, 't) out -> 'v -> 't IO.io
  val receive : 'var inp -> 'var IO.io
  val close : close -> unit IO.io
  val send_many : ('v, 't) scatter -> (int -> 'v) -> 't IO.io
  val receive_many : 'var gather -> 'var IO.io
  
  type 't global
  type 't tup
  
  val ( --> ) :
    ('a one, 'b one, 'c, 'd, 'e, 'f inp) role ->
    ('g one, 'e one, 'h, 'c, 'b, 'i) role ->
    ('i, ('j, 'a) out, 'f, 'j * 'g lin) label -> 'h global -> 'd global
  
  val gather :
    ('a list, 'b list, 'c, 'd, 'e, 'f gather) role ->
    ('g one, 'e one, 'h, 'c, 'b, 'i) role ->
    ('i, ('j, 'a) out, 'f, 'j list * 'g lin) label ->
    'h global -> 'd global
  
  val scatter :
    ('a one, 'b one, 'c, 'd, 'e, 'f inp) role ->
    ('g list, 'e list, 'h, 'c, 'b, 'i) role ->
    ('i, ('j, 'a) scatter, 'f, 'j * 'g lin) label ->
    'h global -> 'd global
  
  val choice_at :
    ('a one, 'b one, 'c, 'd, 'e, 'f) role ->
    ('b, 'g, 'h) disj ->
    ('g one, unit one, 'i, 'c, 'j, 'k) role * 'i global ->
    ('h one, unit one, 'm, 'c, 'n, 'o) role * 'm global ->
    'd global
  
  val fix : ('g global -> 'g global) -> 'g global
  
  val finish : ([ `cons of close one * 'a ] as 'a) global
  
  val finish_with_multirole :
    at:(close one, close list, [ `cons of close one * 'a ] as 'a, 'g, _, _) role ->
    'g global
  
  val with_multirole :
    at:(close one, close list, 'g0, 'g1, 'a, 'b) role ->
    'g0 global -> 'g1 global
  
  val closed_at :
    (close one, close one, 'g, 'g, 'a, 'b) role ->
    'g global -> 'g global
  
  val closed_list_at :
    (close list, close list, 'g, 'g, 'a, 'b) role ->
    'g global -> 'g global
  
  val gen_with_env : Env.t -> 'a global -> 'a tup
  
  val gen : 'a global -> 'a tup
  
  val gen_mult : int list -> 'a global -> 'a tup
  
  val get_ch : ('a one, 'b, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a
  
  val get_ch_list : ('a list, 'b, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a list
  
  val get_ch_ : ('a one, unit one, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a * 'd tup
  
  val get_ch_list_ : ('a list, unit one, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a list * 'd tup
  
  type 'a ty
  
  val get_ty : ('a one, 'b, 'c, 'd, 'e, 'f) role -> 'c global -> 'a ty
  
  val get_ty_ : ('a one, 'b, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a ty
  
  val get_ty_list : ('a list, 'b, 'c, 'd, 'e, 'f) role -> 'c global -> 'a ty
  
  val get_ty_list_ : ('a list, 'b, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a ty
  
  val (>:) :
    ('obj,('v, 'epA) out, 'var, 'v * 'epB) label ->
    'v ty ->
    ('obj,('v, 'epA) out, 'var, 'v * 'epB) label
  
  val (>>:) :
    ('obj,('v, 'epA) scatter, 'var, 'v * 'epB) label ->
    'v ty ->
    ('obj,('v, 'epA) scatter, 'var, 'v * 'epB) label
      
  val effective_length : 't tup -> int
  
  val env : 't tup -> Env.t
end = struct

  module Seq = Seq.Make(struct type 'a t = 'a DynLin.gen end)
  module Single = Comm_base.Single(DynLin)
  module ScatterGather = Comm_base.ScatterGather(DynLin)

  type 't global = Env.t -> 't Seq.t

  type 'a lin = 'a Lin.lin
  
  type close_ = Single.close
  
  (**
   * Communication combinator
   *)
  let (-->) ri rj label (g0 : _ global) : _ global = fun env ->
    let g0 = g0 env in
    let sj' = Seq.get rj.role_index g0 in
    let wrap = fun x ->
      label.var (x, Lin.mklin @@ DynLin.fresh @@ Mergeable.resolve sj')
    in
    let out, inp = Name.create wrap in
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
    let wrap = fun x ->
      label.var (x, Lin.mklin @@ DynLin.fresh (Mergeable.resolve sj'))
    in
    let outs, gather = Name.create_gather count wrap in
    let sj = ScatterGather.declare_gather ri.role_label gather sj' in
    let g1 = Seq.put rj.role_index g0 sj in
    let si' = Seq.get_list ~size:count ri.role_index g1 in
    let si = List.map2 (Single.declare_out rj.role_label label.obj) outs si' in
    let g2 = Seq.put_list ri.role_index g1 si in
    g2
  
  let scatter ri rj label (g0 : _ global) : _ global = fun env ->
    let count = Env.rm_size env (int_of_idx rj.role_index) in
    let g0 = g0 env in
    let sj' = Seq.get_list rj.role_index ~size:count g0 in
    let wrap i =
      let sj' = List.nth sj' i in
      (fun x -> label.var (x, Lin.mklin @@ DynLin.fresh (Mergeable.resolve sj')))
    in
    let scatter, inps = Name.create_scatter count wrap in
    let sj = List.map2 (Single.declare_inp ri.role_label) inps sj' in
    let g1 = Seq.put_list rj.role_index g0 sj in
    let si' = Seq.get ri.role_index g1 in
    let si = ScatterGather.declare_scatter rj.role_label label.obj scatter si' in
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
  
  let fix : type g. (g global -> g global) -> g global = fun f env ->
    let rec body =
      lazy ((f ((fun _ -> Seq.recvar body))) env)
    in
    (* A "fail-fast" approach to detect unguarded loops.
     * Seq.partial_force tries to fully evaluate unguarded recursion variables
     * in the body.
    *)
    Seq.resolve_merge (Lazy.force body)
  
  let finish_seq =
    Seq.repeat 0 (fun _ -> Single.declare_close)
  
  let finish : ([`cons of close_ one * 'a] as 'a) global = fun _ ->
    finish_seq
  
  let finish_with_multirole :
      at:(close_ one, close_ list, [ `cons of close_ one * 'a ] as 'a, 'g, _, _) role ->
      'g global = fun ~at env ->
    let count = Env.rm_size env (int_of_idx at.role_index) in
    let g' = Seq.put_list at.role_index finish_seq (List.init count (fun _ -> Single.declare_close)) in
    g'
  
  let closed_at : 'g. (close_ one, close_ one, 'g, 'g, _, _) role -> 'g global -> 'g global
    = fun r g env ->
      let g = g env in
      let g' = Seq.put r.role_index g Single.declare_close in
      g'
  
  let closed_list_at_ r g env =
      let g = g env in
      let count = Env.rm_size env (int_of_idx r.role_index) in
      let g' = Seq.put_list r.role_index g (List.init count (fun _ -> Single.declare_close)) in
      g'
  
  let closed_list_at : 'g. (close_ list, close_ list, 'g, 'g, _, _) role -> 'g global -> 'g global
    = closed_list_at_
  
  let with_multirole ~at = closed_list_at_ at
  
  type 't tup = Env.t * 't Seq.t
  
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
    let seq' = Seq.put role.role_index seq munit in
    ch, (env, seq')

  let get_ch_list_ role ((env, seq) as tup) =
    let ch = get_ch_list role tup in
    let seq' = Seq.put role.role_index seq munit in
    ch, (env, seq')
  
  type 'a ty =
      Ty__ of (unit -> 'a)
    | TyList__ of (unit -> 'a list)
  
  let get_ty_ = fun r g ->
    Ty__ (fun () -> get_ch r g)
  
  let get_ty = fun r g ->
    get_ty_ r (gen g)
  
  let get_ty_list_ = fun r g ->
    TyList__ (fun () -> get_ch_list r g)
  
  let get_ty_list = fun r g ->
    get_ty_list_ r (gen g)
  
  let (>:) l _ = l
  
  let (>>:) l _ = l
  
  let env (env, _) = env
  
  include Single
  include ScatterGather
end

module Dyn = Make(DynLin.Check)(NoStatic)
