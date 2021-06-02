(**
  The module for wrapper. 
  A wrapper is a function which wraps input channels, of form [λx.`A(x,c)]
  where [c] is a continuation channel vector after reception.
  Merging wrapped channels enables external choices on multiple input channels.
  Herefter [ch@w] denotes a channel [ch] wrapped by [w].
  For example, waiting on multiple wrapped channels [[ch1@λx.`A(x,c1); ch2@λx.`B(x,c2)]] 
  yields either [`A(v1,c1)] or [`B(v2,c2)] depending on the value [vi] received from [chi].

  [Wrapper.make constr cont] returns a pair of wrapper function [λx.`Constr(x,cont)] and 
  internal state of the wrapper, where [constr] denotes a variant constructor of type
  [([>`Constr of 'v * 'c], 'v * 'c) constr] and a contunuation [cont] of type ['c Mergeable.t].

  In a previous version of ocaml-mpst, in a very rare case when waiting on the same variant tag `C 
  on the same channel results in a non-deterministic behaviour, and to avoid this,
  this [Wrapper] module has an elaborated behaviour on merging.
 *)
 
 open Types

module Make(X:sig type 'a t and 'a u val fresh : 'a t -> 'a u val unfresh_ : 'a u -> 'a t end) : sig

  type 'var t

  val merge : 'var t list -> 'var t list -> 'var t list

  val make : ('var, 'v * 'c X.u) constr -> 'c X.t Mergeable.t -> ('v -> 'var) ref * 'var t

end= struct

  (** The manipulator of a wrapper of form [`A(_,c)], to merge [c] with other wrapper having the same tag `A *)
  type 'var t =
    {
     mutable other_wrappers: 'var t list;
     (** Other wrappers of same input label type. *)

     merge: 'var -> 'var option;
     (** Merge the channel vector inside the variant tag.
         If the constructor of the passed argument is same as this wrapper, 
         i.e. if this wrapper is of form [`A(_,c)] and the argument is [`A(v,c2)],
         then extract the continuation channel vector [c2], and returns the new variant
         with the merged channel vector [`A(v,c1 ⊔ c2)].
      *)

     set_cont: 'var -> unit;
     (** Initialise the wrapper by setting the continuation inside the given argument,
         if it is of form [`A(v,c)] (where `A is the same label as this wrapper).
         It extracts the continuation [c] and update the above [wrapperfun] accordingly,
         thus memoises the continuation for the wrapper which are called more than twice.
      *)
    }

  let merge (w1:'var t list) (w2:'var t list) : 'var t list =
    let wrappers = w1 @ w2 in
    (* FIXME losing w.other_wrappers *)
    List.iter (fun w -> w.other_wrappers <- List.filter (fun w' -> w != w') wrappers) wrappers;
    wrappers


  let merge0 (cont:'a X.t Mergeable.t) (c:'a X.u) =
    Mergeable.resolve @@ 
      Mergeable.merge 
         cont 
         (Mergeable.make ~value:(X.unfresh_ c) ~mergefun:(Mergeable.mergefun_ cont) ())

  (** The wrapper function which takes payload [v : 'v] and returns [`A(v, c) : 'var],
      where [c : 'c] is the continuation channel vector.
      It checks other_wrappers having the same variant tag of form [`A(_,ci)], 
      and if so, collect the continuation channel vectors and merge them, and returns
      the variant with merged continuation [`A(v,c⊔c1⊔..⊔cN)].
      The merged continuation is cached and  from the second time.
  *)
  let make constr cont =
    let initialise_wrapper self = fun v ->
      (* Get the variant of form `A(v,c)  *)
      let var = constr.make_var (v, X.fresh @@ Mergeable.resolve cont) in
      let var = 
        (* Repeatedly merge with other channel vectors inside wrappers `A(_,c') having same tag `A *)
        List.fold_left 
          (fun var otherw -> match otherw.merge var with Some var' -> var' | None -> var) 
          var self.other_wrappers
      in
      (* And initialise wrappers having the same tag. *)
      List.iter (fun wrapper -> wrapper.set_cont var) self.other_wrappers;
      self.set_cont var;
      ()
    in
    let wrapperfun = ref (fun _ -> assert false) in
    let set_cont var =
      (* Initialise the wrapper with a proper continuation. See above. *)
      match constr.match_var var with
      | Some (_,cont') -> 
        (* hack to recover linearity checking -- merge function knows how to recover linearity *)
        let cont' = Mergeable.make ~value:(X.unfresh_ cont') ~mergefun:(Mergeable.mergefun_ cont) () in
        let cont' = Mergeable.resolve @@ Mergeable.merge cont' cont' in
         wrapperfun := (fun v -> constr.make_var (v, X.fresh cont'));
      | None -> ()
    in
    let merge var =
      (* Merging of channel vector inside the variant tag. See above. *)
      match constr.match_var var with
      | Some (v,c) -> Some (constr.make_var (v, X.fresh @@ merge0 cont c))
      | None -> None
    in
    let wrapper = {set_cont; merge; other_wrappers=[]} in
    (* let the wrapper initialise itself (if not initialised by other wrapper having the same tag) *)
    wrapperfun := (fun v -> initialise_wrapper wrapper v; !wrapperfun v);
    wrapperfun, wrapper
end
