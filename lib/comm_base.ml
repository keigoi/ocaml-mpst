open Concur_shims
open Base

module Single(Local:S.LOCAL)(DynLin:DynLin.S) : sig
  type 'a local = 'a DynLin.gen Mergeable.t

  type ('v, 's) out
  (** Bare output channel. *)

  type 'var inp
  (** Bare input channel. *)

  type close

  val declare_out :
    ('obj, 'm) method_ ->
    ('m, ('v, 's) out) method_ ->
    'v Local.out -> 's local ->
    'obj local    

  val declare_inp :
    ('obj, 'var inp) method_ ->
    'var Local.inp -> 's local -> 'obj local

  val declare_close : unit -> close local
  (** (Internal) Final state of a channel. *)

  val send : ('v, 't) out -> 'v -> 't IO.io
  (** Output a value on a bare channel. *)

  val receive : 'var inp -> 'var IO.io
  (** Input a value on a bare channel. *)

  val close : close -> unit IO.io
  (** Close a channel *)

end = struct
  type 'a lin = 'a DynLin.lin
  type 'a local = 'a DynLin.gen Mergeable.t

  type ('v, 's) out = ('v Local.out * 's local) lin
  type 'var inp = 'var Local.inp lin
  type close = unit lin
      
  let declare_out role label out cont =
    let ch =
      (* <role_rj = <lab = (ch,si) > > *)
      DynLin.wrap role.make_obj @@
        DynLin.wrap label.make_obj @@
          DynLin.declare (out,cont)
    in
    let merge_out (n1,s1') (n2,s2') =
      (Local.merge_out n1 n2, Mergeable.merge s1' s2')
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
    let merge_inp_lin = DynLin.merge Local.merge_inp role in
    Mergeable.make
      ~value:ch
      ~cont:cont
      ~mergefun:merge_inp_lin
      ()

  let declare_close () =
    Mergeable.make
      ~value:(DynLin.declare ())
      ~mergefun:(fun _ _ -> DynLin.declare ())
      ()

  let (let*) = IO.bind

  let send (out: ('v,'t) out) (v:'v) =
    let (n,t) = DynLin.use out in
    let* () = Local.send n v in
    IO.return @@ DynLin.fresh (Mergeable.resolve t)

  let receive (ch: 'var inp) =
    let ch = DynLin.use ch in
    Local.receive ch

  let close (ep: unit lin) =
    DynLin.use ep;
    IO.return ()
end


module ScatterGather(Local:S.LOCAL)(DynLin:DynLin.S) : sig
  type 'a local = 'a DynLin.gen Mergeable.t

  type ('v, 's) scatter
  type 'var gather

  val declare_scatter :
    ('obj, 'm) method_ ->
    ('m, ('v, 's) scatter) method_ ->
    'v Local.scatter -> 's local ->
    'obj local

  val declare_gather :
    ('obj, 'var gather) method_ ->
    'var Local.gather -> 's local -> 'obj local
      
  val send_many : ('v, 't) scatter -> (int -> 'v) -> 't IO.io

  val receive_many : 'var gather -> 'var IO.io

end = struct
  type 'a local = 'a DynLin.gen Mergeable.t
  type 'a lin = 'a DynLin.lin

  type ('v, 's) scatter = ('v Local.scatter * 's local) lin
  type 'var gather = 'var Local.gather lin

  let declare_scatter role label out cont =
    let ch =
      (* <role_rj = <lab = (ch,si) > > *)
      DynLin.wrap role.make_obj @@
        DynLin.wrap label.make_obj @@
          DynLin.declare (out,cont)
    in
    let merge_scatter (n1,s1') (n2,s2') =
      (Local.merge_scatter n1 n2, Mergeable.merge s1' s2')
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
    let merge_gather_lin = DynLin.merge Local.merge_gather role in
    Mergeable.make
      ~value:ch
      ~cont:cont
      ~mergefun:merge_gather_lin
      ()

  let (let*) = IO.bind

  let send_many (scatter: ('v,'t) scatter) f =
    let (n,t) = DynLin.use scatter in
    let* () = Local.send_many n f in
    IO.return @@ DynLin.fresh (Mergeable.resolve t)

  let receive_many (ch: 'var gather) =
    let ch = DynLin.use ch in
    Local.receive_many ch
end
