open Concur_shims
open Types

module type LOW_COMM = sig
  type 'a out
  type 'a inp
  type 'a out_many
  type 'a inp_many

  val merge_out : 'a out -> 'a out -> 'a out
  val merge_inp : 'a inp -> 'a inp -> 'a inp

  val send : 'a out -> 'a -> unit IO.io
  val receive : 'a inp -> 'a IO.io

  val merge_out_many : 'a out_many -> 'a out_many -> 'a out_many
  val send_many : 'a out_many -> (int -> 'a) -> unit IO.io

  val merge_inp_many : 'a inp_many -> 'a inp_many -> 'a inp_many
  val receive_many : 'a inp_many -> 'a IO.io
end
   
module MakeSingle(Low:LOW_COMM)(DynLin:Dyn_lin.S) : sig
  type 'a local = 'a DynLin.gen Mergeable.t

  type ('v, 's) out
  (** Bare output channel. *)

  type 'var inp
  (** Bare input channel. *)

  type close

  val declare_out :
    ('obj, 'm) method_ ->
    ('m, ('v, 's) out) method_ ->
    'v Low.out -> 's local ->
    'obj local

  val declare_inp :
    ('obj, 'var inp) method_ ->
    'var Low.inp -> 's local -> 'obj local

  val declare_close : (unit -> unit IO.io) -> close local
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

  type ('v, 's) out = ('v Low.out * 's local) lin
  type 'var inp = 'var Low.inp lin
  type close = (unit -> unit IO.io) lin

  let mergeopt m x y = if x==y then x else m x y

  let declare_out role label out cont =
    let ch =
      (* <role_rj = <lab = (ch,si) > > *)
      DynLin.wrap role.make_obj @@
        DynLin.wrap label.make_obj @@
          DynLin.declare (out,cont)
    in
    let merge_out (n1,s1') (n2,s2') =
      (Low.merge_out n1 n2, Mergeable.merge s1' s2')
    in
    let merge_out_lin x y = DynLin.merge (mergeopt merge_out) (Base.compose_method role label) x y in
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
    let merge_inp_lin x y = DynLin.merge (mergeopt Low.merge_inp) role x y in
    Mergeable.make
      ~value:ch
      ~cont:cont
      ~mergefun:merge_inp_lin
      ()

  let declare_close f =
    Mergeable.make
      ~value:(DynLin.declare f)
      ~mergefun:(fun _ _ -> DynLin.declare f)
      ()

  let[@inline] send (out: ('v,'t) out) (v:'v) =
    IO.bind (DynLin.use out) (fun[@inline] (n,t) ->
    IO.bind (Low.send n v) (fun[@inline] () ->
    IO.return @@ DynLin.fresh (Mergeable.resolve t)))

  let[@inline] receive (ch: 'var inp) =
    IO.bind (DynLin.use ch) (fun[@inline] ch ->
    Low.receive ch)

  let[@inline] close (ep: close) =
    IO.bind (DynLin.use ep) (fun[@inline] f ->
    f ())
end[@@inline]


module MakeScatterGather(Local:LOW_COMM)(DynLin:Dyn_lin.S) : sig
  type 'a local = 'a DynLin.gen Mergeable.t

  type ('v, 's) out_many
  type 'var inp_many

  val declare_out_many :
    ('obj, 'm) method_ ->
    ('m, ('v, 's) out_many) method_ ->
    'v Local.out_many -> 's local ->
    'obj local

  val declare_inp_many :
    ('obj, 'var inp_many) method_ ->
    'var Local.inp_many -> 's local -> 'obj local

  val send_many : ('v, 't) out_many -> (int -> 'v) -> 't IO.io

  val receive_many : 'var inp_many -> 'var IO.io

end = struct
  type 'a local = 'a DynLin.gen Mergeable.t
  type 'a lin = 'a DynLin.lin

  type ('v, 's) out_many = ('v Local.out_many * 's local) lin
  type 'var inp_many = 'var Local.inp_many lin

  let mergeopt m x y = if x==y then x else m x y

  let declare_out_many role label out cont =
    let ch =
      (* <role_rj = <lab = (ch,si) > > *)
      DynLin.wrap role.make_obj @@
        DynLin.wrap label.make_obj @@
          DynLin.declare (out,cont)
    in
    let merge_out_many (n1,s1') (n2,s2') =
      (Local.merge_out_many n1 n2, Mergeable.merge s1' s2')
    in
    let merge_out_many_lin = DynLin.merge (mergeopt merge_out_many) (Base.compose_method role label) in
    Mergeable.make
      ~value:ch
      ~cont:cont
      ~mergefun:merge_out_many_lin
      ()

  let declare_inp_many role inp cont =
    let ch =
      (* <role_ri = wrap (receive ch) (λx.`lab(x,sj)) > *)
      DynLin.wrap role.make_obj @@
        DynLin.declare inp
    in
    let merge_inp_many_lin = DynLin.merge (mergeopt Local.merge_inp_many) role in
    Mergeable.make
      ~value:ch
      ~cont:cont
      ~mergefun:merge_inp_many_lin
      ()

  let (let*) = IO.bind

  let send_many (out_many: ('v,'t) out_many) f =
    let* (n,t) = DynLin.use out_many in
    let* () = Local.send_many n f in
    IO.return @@ DynLin.fresh (Mergeable.resolve t)

  let receive_many (ch: 'var inp_many) =
    let* ch = DynLin.use ch in
    Local.receive_many ch
end[@@inline]
