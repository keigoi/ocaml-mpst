open Base
open Concur_shims

module Seq = Seq.Make(struct type 'a t = 'a DynLin.gen end)

let (let*) = IO.bind

type 'a lin = 'a DynLin.lin
type 'a gen = 'a DynLin.gen

module Single : sig
  type ('v, 's) out = ('v Name.out * 's gen Mergeable.t) lin
  (** Bare output channel. *)

  type 'var inp = 'var Name.inp lin
  (** Bare input channel. *)

  type close = unit lin

  val declare_out :
    ('obj, 'm) method_ ->
    ('m, ('v, 's) out) method_ ->
    'v Name.out -> 's gen Mergeable.t ->
    'obj gen Mergeable.t    

  val declare_inp :
    ('obj, 'var inp) method_ ->
    'var Name.inp -> 's gen Mergeable.t -> 'obj gen Mergeable.t

  val declare_close : unit lin gen Mergeable.t
  (** (Internal) Final state of a channel. *)

  val send : ('v, 't) out -> 'v -> 't IO.io
  (** Output a value on a bare channel. *)

  val receive : 'var inp -> 'var IO.io
  (** Input a value on a bare channel. *)

  val close : close -> unit IO.io
  (** Close a channel *)

end = struct
  type ('v, 's) out = ('v Name.out * 's gen Mergeable.t) lin
  type 'var inp = 'var Name.inp lin
  type close = unit lin
      
  let declare_out role label out cont =
    let ch =
      (* <role_rj = <lab = (ch,si) > > *)
      DynLin.wrap role.make_obj @@
        DynLin.wrap label.make_obj @@
          DynLin.declare (out,cont)
    in
    let merge_out (n1,s1') (n2,s2') =
      (Name.merge_out n1 n2, Mergeable.merge s1' s2')
    in
    let merge_out_lin = DynLin.merge merge_out (compose_method role label) in
    Mergeable.make
      ~value:ch
      ~cont:cont
      ~mergefun:merge_out_lin
      ()
      
  let declare_inp role inp cont =
    let ch =
      (* <role_ri = wrap (receive ch) (λx.`lab(x,sj)) > *)
      DynLin.wrap role.make_obj @@
        DynLin.declare inp
    in
    let merge_inp_lin = DynLin.merge Name.merge_inp role in
    Mergeable.make
      ~value:ch
      ~cont:cont
      ~mergefun:merge_inp_lin
      ()

  let declare_close =
    Mergeable.make
      ~value:(DynLin.declare ())
      ~mergefun:(fun _ _ -> DynLin.declare ())
      ()

  let send (out: ('v,'t) out) (v:'v) =
    let* (n,t) = DynLin.use out in
    let* () = Name.send n v in
    IO.return @@ DynLin.fresh (Mergeable.resolve t)

  let receive (ch: 'var inp) =
    let* ch = DynLin.use ch in
    Name.receive ch

  let close (ep: unit lin) =
    DynLin.use ep
end

module ScatterGather : sig
  type ('v, 's) scatter = ('v Name.scatter * 's gen Mergeable.t) lin
  type 'var gather = 'var Name.gather lin

  val declare_scatter :
    ('obj, 'm) method_ ->
    ('m, ('v, 's) scatter) method_ ->
    'v Name.scatter -> 's gen Mergeable.t ->
    'obj gen Mergeable.t    

  val declare_gather :
    ('obj, 'var gather) method_ ->
    'var Name.gather -> 's gen Mergeable.t -> 'obj gen Mergeable.t
      
  val send_many : ('v, 't) scatter -> (int -> 'v) -> 't IO.io

  val receive_many : 'var gather -> 'var IO.io

end = struct

  type ('v, 's) scatter = ('v Name.scatter * 's gen Mergeable.t) lin
  type 'var gather = 'var Name.gather lin

  let declare_scatter role label out cont =
    let ch =
      (* <role_rj = <lab = (ch,si) > > *)
      DynLin.wrap role.make_obj @@
        DynLin.wrap label.make_obj @@
          DynLin.declare (out,cont)
    in
    let merge_scatter (n1,s1') (n2,s2') =
      (Name.merge_scatter n1 n2, Mergeable.merge s1' s2')
    in
    let merge_scatter_lin = DynLin.merge merge_scatter (compose_method role label) in
    Mergeable.make
      ~value:ch
      ~cont:cont
      ~mergefun:merge_scatter_lin
      ()
      
  let declare_gather role inp cont =
    let ch =
      (* <role_ri = wrap (receive ch) (λx.`lab(x,sj)) > *)
      DynLin.wrap role.make_obj @@
        DynLin.declare inp
    in
    let merge_gather_lin = DynLin.merge Name.merge_gather role in
    Mergeable.make
      ~value:ch
      ~cont:cont
      ~mergefun:merge_gather_lin
      ()

  let send_many (scatter: ('v,'t) scatter) f =
    let* (n,t) = DynLin.use scatter in
    let* () = Name.send_many n f in
    IO.return @@ DynLin.fresh (Mergeable.resolve t)

  let receive_many (ch: 'var gather) =
    let* ch = DynLin.use ch in
    Name.receive_many ch
end

include Single
include ScatterGather

type 't global = Env.t -> 't Seq.t

(**
 * Communication combinator
 *)
let (-->) ri rj label (g0 : _ global) : _ global = fun env ->
  let g0 = g0 env in
  let sj' = Seq.get rj.role_index g0 in
  let out, inp = Name.create (fun x -> label.var (x, DynLin.fresh (Mergeable.resolve sj'))) in
  let sj = declare_inp ri.role_label inp sj' in
  let g1 = Seq.put rj.role_index g0 sj in
  let si' = Seq.get ri.role_index g1 in
  let si = declare_out rj.role_label label.obj out si' in
  let g2 = Seq.put ri.role_index g1 si in
  g2

let gather ri rj label (g0 : _ global) : _ global = fun env ->
  let count = Env.rm_size env (int_of_idx ri.role_index) in
  let g0 = g0 env in
  let sj' = Seq.get rj.role_index g0 in
  let outs, gather = Name.create_gather count (fun x -> label.var (x, DynLin.fresh (Mergeable.resolve sj'))) in
  let sj = declare_gather ri.role_label gather sj' in
  let g1 = Seq.put rj.role_index g0 sj in
  let si' = Seq.get_list ~size:count ri.role_index g1 in
  let si = List.map2 (declare_out rj.role_label label.obj) outs si' in
  let g2 = Seq.put_list ri.role_index g1 si in
  g2

let scatter ri rj label (g0 : _ global) : _ global = fun env ->
  let count = Env.rm_size env (int_of_idx rj.role_index) in
  let g0 = g0 env in
  let sj' = Seq.get_list rj.role_index ~size:count g0 in
  let wrap i =
    let sj' = List.nth sj' i in
    (fun x -> label.var (x, DynLin.fresh (Mergeable.resolve sj')))
  in
  let scatter, inps = Name.create_scatter count wrap in
  let sj = List.map2 (declare_inp ri.role_label) inps sj' in
  let g1 = Seq.put_list rj.role_index g0 sj in
  let si' = Seq.get ri.role_index g1 in
  let si = declare_scatter rj.role_label label.obj scatter si' in
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
  Seq.repeat 0 (fun _ -> declare_close)

let finish : ([`cons of close one * 'a] as 'a) global = fun _ ->
  finish_seq

let finish_with_multirole :
    at:(close one, close list, [ `cons of close one * 'a ] as 'a, 'g, _, _) role ->
    'g global = fun ~at env ->
  let count = Env.rm_size env (int_of_idx at.role_index) in
  let g' = Seq.put_list at.role_index finish_seq (List.init count (fun _ -> declare_close)) in
  g'

let closed_at : 'g. (close one, close one, 'g, 'g, _, _) role -> 'g global -> 'g global
  = fun r g env ->
    let g = g env in
    let g' = Seq.put r.role_index g declare_close in
    g'

let closed_list_at_ r g env =
    let g = g env in
    let count = Env.rm_size env (int_of_idx r.role_index) in
    let g' = Seq.put_list r.role_index g (List.init count (fun _ -> declare_close)) in
    g'

let closed_list_at : 'g. (close list, close list, 'g, 'g, _, _) role -> 'g global -> 'g global
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
