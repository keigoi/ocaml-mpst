open Rows
open State

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
  open OutMerge
  module InpMerge = Inp.Make (Name)
  open InpMerge

  type 's out = 's OutMerge.out
  type 'var inp = 'var InpMerge.inp

  let out role lab name s =
    State.deterministic (StateHash.make_key ())
    @@ Lazy.from_val
         {
           body =
             role.make_obj
             @@ lab.make_obj
             @@ Out (lab.method_name, name, Lazy.from_val s);
           determinise_list = OutMerge.out_determinise role lab;
           force_traverse = OutMerge.out_force role lab;
           to_string = OutMerge.out_to_string role lab;
         }

  let inp role constr name s =
    State.deterministic (StateHash.make_key ())
    @@ Lazy.from_val
         {
           body =
             role.make_obj @@ lazy (name, [ ExternalChoiceItem (constr, s) ]);
           determinise_list = InpMerge.inp_determinise role;
           force_traverse = InpMerge.inp_force role;
           to_string = InpMerge.inp_to_string role;
         }
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
