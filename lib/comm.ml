open Base
open Lin
open Concur_shims

let (let*) = IO.bind
let ret = IO.return

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
      Lin.wrap role.make_obj @@
        Lin.wrap label.make_obj @@
          Lin.declare (out,cont)
    in
    let merge_out (n1,s1') (n2,s2') =
      (Name.merge_out n1 n2, Mergeable.merge s1' s2')
    in
    let merge_out_lin = Lin.merge merge_out (compose_method role label) in
    Mergeable.make
      ~value:ch
      ~cont:cont
      ~mergefun:merge_out_lin
      ()
      
  let declare_inp role inp cont =
    let ch =
      (* <role_ri = wrap (receive ch) (λx.`lab(x,sj)) > *)
      Lin.wrap role.make_obj @@
        Lin.declare inp
    in
    let merge_inp_lin = Lin.merge Name.merge_inp role in
    Mergeable.make
      ~value:ch
      ~cont:cont
      ~mergefun:merge_inp_lin
      ()

  let declare_close =
    Mergeable.make
      ~value:(Lin.declare ())
      ~mergefun:(fun _ _ -> Lin.declare ())
      ()

  let send (out: ('v,'t) out) (v:'v) =
    let* (n,t) = Lin.use out in
    let* () = Name.send n v in
    ret @@ Lin.fresh (Mergeable.resolve t)

  let receive (ch: 'var inp) =
    let* ch = Lin.use ch in
    Name.receive ch

  let close (ep: unit lin) =
    Lin.use ep
end

module ScatterGather : sig
  type ('v, 's) scatter
  type 'var gather

  val declare_scatter :
    ('obj, 'm) method_ ->
    ('m, ('v, 's) scatter) method_ ->
    'v Name.scatter -> 's gen Mergeable.t ->
    'obj gen Mergeable.t    

  val declare_gather :
    ('obj, 'var gather) method_ ->
    'var Name.gather -> 's gen Mergeable.t -> 'obj gen Mergeable.t
      
  val send_list : ('v, 't) scatter -> (int -> 'v) -> 't IO.io

  val receive_list : 'var gather -> 'var IO.io

end = struct

  type ('v, 's) scatter = ('v Name.scatter * 's gen Mergeable.t) lin
  type 'var gather = 'var Name.gather lin

  let declare_scatter role label out cont =
    let ch =
      (* <role_rj = <lab = (ch,si) > > *)
      Lin.wrap role.make_obj @@
        Lin.wrap label.make_obj @@
          Lin.declare (out,cont)
    in
    let merge_scatter (n1,s1') (n2,s2') =
      (Name.merge_scatter n1 n2, Mergeable.merge s1' s2')
    in
    let merge_scatter_lin = Lin.merge merge_scatter (compose_method role label) in
    Mergeable.make
      ~value:ch
      ~cont:cont
      ~mergefun:merge_scatter_lin
      ()
      
  let declare_gather role inp cont =
    let ch =
      (* <role_ri = wrap (receive ch) (λx.`lab(x,sj)) > *)
      Lin.wrap role.make_obj @@
        Lin.declare inp
    in
    let merge_gather_lin = Lin.merge Name.merge_gather role in
    Mergeable.make
      ~value:ch
      ~cont:cont
      ~mergefun:merge_gather_lin
      ()

  let send_list (scatter: ('v,'t) scatter) f =
    let* (n,t) = Lin.use scatter in
    let* () = Name.send_list n f in
    ret @@ Lin.fresh (Mergeable.resolve t)

  let receive_list (ch: 'var gather) =
    let* ch = Lin.use ch in
    Name.receive_list ch
end

type 't global = Env.t -> 't Seq.t

(**
 * Communication combinator
 *)
let (-->) ri rj label (g0 : _ global) : _ global = fun env ->
  let g0 = g0 env in
  let sj' = Seq.get rj.role_index g0 in
  let out, inp = Name.create (fun x -> label.var (x, Lin.fresh (Mergeable.resolve sj'))) in
  let sj = Single.declare_inp ri.role_label inp sj' in
  let g1 = Seq.put rj.role_index g0 sj in
  let si' = Seq.get ri.role_index g1 in
  let si = Single.declare_out rj.role_label label.obj out si' in
  let g2 = Seq.put ri.role_index g1 si in
  g2

let gather ri rj label (g0 : _ global) : _ global = fun env ->
  let si_info = Table.get env.metainfo (int_of_idx ri.role_index) in
  let count = si_info.rm_size in
  let g0 = g0 env in
  let sj' = Seq.get rj.role_index g0 in
  let outs, gather = Name.create_gather count (fun x -> label.var (x, Lin.fresh (Mergeable.resolve sj'))) in
  let sj = ScatterGather.declare_gather ri.role_label gather sj' in
  let g1 = Seq.put rj.role_index g0 sj in
  let si' = Seq.get_list ~size:count ri.role_index g1 in
  let si = List.map2 (Single.declare_out rj.role_label label.obj) outs si' in
  let g2 = Seq.put_list ri.role_index g1 si in
  g2

let scatter ri rj label (g0 : _ global) : _ global = fun env ->
  let sj_info = Table.get env.metainfo (int_of_idx rj.role_index) in
  let count = sj_info.rm_size in
  let g0 = g0 env in
  let sj' = Seq.get_list rj.role_index ~size:count g0 in
  let wrap i =
    let sj' = List.nth sj' i in
    (fun x -> label.var (x, Lin.fresh (Mergeable.resolve sj')))
  in
  let scatter, inps = Name.create_scatter count wrap in
  let sj = List.map2 (Single.declare_inp ri.role_label) inps sj' in
  let g1 = Seq.put_list rj.role_index g0 sj in
  let si' = Seq.get ri.role_index g1 in
  let si = ScatterGather.declare_scatter rj.role_label label.obj scatter si' in
  let g2 = Seq.put ri.role_index g1 si in
  g2

let munit =
  let unit = Lin.declare_unlimited () in
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
      let disj = Lin.lift_disj disj in (* lift *)
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

let finish : ([`cons of Single.close one * 'a] as 'a) global = fun _ ->
  Seq.repeat 0 (fun _ -> Single.declare_close)

let closed : 'g. (Single.close one, Single.close one, 'g, 'g, _, _) role -> 'g global -> 'g global
  = fun r g env ->
    let g = g env in
    let g' = Seq.put r.role_index g Single.declare_close in
    g'

let gen_with_param env g = (env, g env)

type 't tup = Env.t * 't Seq.t

let gen g =
  gen_with_param
    {Env.metainfo=Table.create (); default=(fun _ -> EpLocal)}
    g

let get_ch role (_, g) =
  Lin.fresh @@ Mergeable.resolve @@ Seq.get role.role_index g

let get_ch_list role (env, g) =
  let idx = int_of_idx role.role_index in
  let size = (Table.get env.Env.metainfo idx).rm_size in
  List.map (fun x -> Lin.fresh @@ Mergeable.resolve x)
    @@ Seq.get_list ~size role.role_index g

include Single
include ScatterGather
