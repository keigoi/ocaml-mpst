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

  val close : close -> unit
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
    let (n,t) = Lin.use out in
    let* () = Name.send n v in
    ret @@ resolve t

  let receive (ch: 'var inp) =
    let ch = Lin.use ch in
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

type 't global = env -> 't

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
  let g0 = g0 env in
  let sj' : _ local = Seq.get rj.role_index g0 in
  let outs, gather = Name.create_gather 0 (fun x -> label.var (x, Lin.fresh (Mergeable.resolve sj'))) in
  let sj : _ local = ScatterGather.declare_gather ri.role_label gather sj' in
  let g1 = Seq.put rj.role_index g0 sj in
  let si' : _ local list = Seq.get_list ~size:0 ri.role_index g1 in
  let si : _ local list = List.map2 (Single.declare_out rj.role_label label.obj) outs si' in
  let g2 = Seq.put_list ri.role_index g1 si in
  g2

let scatter ri rj label (g0 : _ global) : _ global = fun env ->
  let g0 = g0 env in
  let sj' : _ local list = Seq.get_list rj.role_index ~size:0 g0 in
  let wrap i =
    let sj' = List.nth sj' i in
    (fun x -> label.var (x, Lin.fresh (Mergeable.resolve sj')))
  in
  let scatter, inps = Name.create_scatter 0 wrap in
  let sj : _ local list = List.map2 (Single.declare_inp ri.role_label) inps sj' in
  let g1 = Seq.put_list rj.role_index g0 sj in
  let si' : _ local = Seq.get ri.role_index g1 in
  let si : _ local = ScatterGather.declare_scatter rj.role_label label.obj scatter si' in
  let g2 = Seq.put ri.role_index g1 si in
  g2

