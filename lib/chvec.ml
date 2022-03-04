open Rows

module type Name = sig
  type 'a name
  type 'a endpoint

  val unify : 'a name -> 'a name -> unit
  val finalise : 'a name -> 'a endpoint
end

type tag = int

module type S = sig
  type 'a name
  type 'a endpoint
  type chan

  val new_name : chan -> 'a name
  val unify : 'a name -> 'a name -> unit
  val finalise : 'a name -> 'a endpoint

  type 'a out
  and 'var inp

  val out :
    ('a, 'b) Rows.method_ ->
    ('b, 'c out) Rows.method_ ->
    tag name ->
    'c State.t ->
    'a State.t

  val inp :
    ('a, 'c inp) Rows.method_ ->
    ('c, 'd) Rows.constr ->
    tag name ->
    'd State.t ->
    'a State.t

  val make : unit -> chan
  val select : 'a out -> 'a
  val branch : 'a inp -> 'a
end

module Make (Name : Name) = struct
  module OutMerge = Out.Make (Name)
  module InpMerge = Inp.Make (Name)

  type 's out = 's OutMerge.out
  type 'var inp = 'var InpMerge.inp

  let out = OutMerge.out
  let inp = InpMerge.inp
end

module Sync = struct
  include Name
  module M = Make (Name)
  include M

  type chan = unit

  let make () = ()

  let select (OutMerge.Out (labname, name, cont)) =
    let tag = Btype.hash_variant labname in
    let cont = State.ensure_determinised (Lazy.force cont) in
    Event.sync (Event.send (Name.finalise name) tag);
    cont

  let branch (inp : _ inp) =
    let make_event (InpMerge.ExternalChoiceItem (var, cont)) =
      let cont = State.ensure_determinised cont in
      (Btype.hash_variant var.constr_name, var.make_var cont)
    in
    let name, items = Lazy.force inp in
    items
    |> List.map make_event
    |> List.assoc (Event.sync (Event.receive (Name.finalise name)))
end

module Async = struct
  include DynChan
  module M = Make (DynChan)
  include M

  let select (OutMerge.Out (labname, name, cont)) =
    let tag = Btype.hash_variant labname in
    let cont = State.ensure_determinised (Lazy.force cont) in
    DynChan.send (DynChan.finalise name) tag;
    cont

  let branch (inp : _ inp) =
    let make_event (InpMerge.ExternalChoiceItem (var, cont)) =
      let cont = State.ensure_determinised cont in
      (Btype.hash_variant var.constr_name, var.make_var cont)
    in
    let name, items = Lazy.force inp in
    items
    |> List.map make_event
    |> List.assoc (DynChan.receive (DynChan.finalise name))
end
