include Base
include Lin

let (let*) = Lwt.bind
let (and*) = Lwt.both
let ret = Lwt.return

let resolve (t : 'a gen Mergeable.t) = Lin.fresh (Mergeable.resolve t)

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

  val send : ('v, 't) out -> 'v -> 't Lwt.t
  (** Output a value on a bare channel. *)

  val receive : 'var inp -> 'var Lwt.t
  (** Input a value on a bare channel. *)

  val close : close -> unit Lwt.t
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
    ret @@ resolve t

  let receive (ch: 'var inp) =
    let* ch = Lin.use ch in
    Name.receive ch

  let close (ep: unit lin) =
    Lin.use ep
end

include Single

type 's local = 's gen Mergeable.t

type epkind =
    EpLocal
  (* | EpDpipe of Dpipe.dpipe list Table.t list
   * | EpUntyped of (Base.tag * Obj.t) EV.channel list Table.t list *)

type role_metainfo =
  {rm_index: int;
   rm_kind: epkind;
   rm_size: int}

type env = {metainfo: role_metainfo Table.t; default:int -> epkind}

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
  let rm_index = int_of_idx role.role_index in
  let rm_size = of_option size ~dflt:(rm_size env rm_index) in
  let rm_kind = rm_kind env rm_index rm_size in
  {rm_index; rm_kind; rm_size}

type 't global = env -> 't Seq.t

type 'a one = 'a Seq.one

(**
 * Communication combinator
 *)
let (-->) ri rj label (g0 : _ global) : _ global = fun env ->
  let g0 = g0 env in
  let sj' : _ local = Seq.get rj.role_index g0 in
  let out, inp = Name.create (fun x -> label.var (x, Lin.fresh (Mergeable.resolve sj'))) in
  let sj : _ local = Single.declare_inp ri.role_label inp sj' in
  let g1 = Seq.put rj.role_index g0 sj in
  let si' : _ local = Seq.get ri.role_index g1 in
  let si : _ local = Single.declare_out rj.role_label label.obj out si' in
  let g2 = Seq.put ri.role_index g1 si in
  g2

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
end

let gather ri rj label (g0 : _ global) : _ global = fun env ->
  let si_info = Table.get env.metainfo (int_of_idx ri.role_index) in
  let count = si_info.rm_size in
  let g0 = g0 env in
  let sj' : _ local = Seq.get rj.role_index g0 in
  let outs, gather = Name.create_gather count (fun x -> label.var (x, Lin.fresh (Mergeable.resolve sj'))) in
  let sj : _ local = ScatterGather.declare_gather ri.role_label gather sj' in
  let g1 = Seq.put rj.role_index g0 sj in
  let si' : _ local list = Seq.get_list ~size:count ri.role_index g1 in
  let si : _ local list = List.map2 (Single.declare_out rj.role_label label.obj) outs si' in
  let g2 = Seq.put_list ri.role_index g1 si in
  g2

let scatter ri rj label (g0 : _ global) : _ global = fun env ->
  let sj_info = Table.get env.metainfo (int_of_idx rj.role_index) in
  let count = sj_info.rm_size in
  let g0 = g0 env in
  let sj' : _ local list = Seq.get_list rj.role_index ~size:count g0 in
  let wrap i =
    let sj' = List.nth sj' i in
    (fun x -> label.var (x, Lin.fresh (Mergeable.resolve sj')))
  in
  let scatter, inps = Name.create_scatter count wrap in
  let sj : _ local list = List.map2 (Single.declare_inp ri.role_label) inps sj' in
  let g1 = Seq.put_list rj.role_index g0 sj in
  let si' : _ local = Seq.get ri.role_index g1 in
  let si : _ local = ScatterGather.declare_scatter rj.role_label label.obj scatter si' in
  let g2 = Seq.put ri.role_index g1 si in
  g2

let munit = Mergeable.make ~value:() ~mergefun:(fun _ _ -> ()) ()

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

let finish : ([`cons of close gen one * 'a] as 'a) global = fun _ ->
  Seq.repeat 0 (fun _ -> Single.declare_close)

let closed : 'g. (close gen one, close gen one, 'g, 'g, _, _) role -> 'g global -> 'g global
  = fun r g env ->
    let g = g env in
    let g' = Seq.put r.role_index g Single.declare_close in
    g'

let gen_with_param p g = g p

let gen g =
  gen_with_param
    {metainfo=Table.create (); default=(fun _ -> EpLocal)}
    g

let get_ch role g =
  Lin.fresh @@ Mergeable.resolve @@ Seq.get role.role_index g
