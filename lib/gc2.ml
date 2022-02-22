open Types

module PolyAssoc (V : sig
  type 'a t
end) : sig
  type 'a key
  type binding = B : 'a key * 'a V.t -> binding

  val newkey : unit -> 'a key
  val empty : binding list
  val lookup : binding list -> 'a key -> 'a V.t
end = struct
  module Key = struct
    type _ t = ..
  end

  module type W = sig
    type t
    type _ Key.t += Key : t Key.t
  end

  type 'a key = (module W with type t = 'a)
  type binding = B : 'a key * 'a V.t -> binding
  type ('a, 'b) eq = Eq : ('a, 'a) eq

  let newkey () (type s) =
    let module M = struct
      type t = s
      type _ Key.t += Key : t Key.t
    end in
    (module M : W with type t = s)

  let eq (type r s) (r : r key) (s : s key) : (r, s) eq option =
    let module R = (val r : W with type t = r) in
    let module S = (val s : W with type t = s) in
    match R.Key with S.Key -> Some Eq | _ -> None

  let empty = []

  let lookup : type a. binding list -> a key -> a V.t =
   fun d k ->
    let rec find : binding list -> a V.t = function
      | [] -> raise Not_found
      | B (k', v) :: bs -> ( match eq k k' with Some Eq -> v | _ -> find bs)
    in
    find d
end

module NameHash_ = PolyAssoc (struct
  type 'a t = 'a State.t
end)

module NameUnify = Unify.Make (NameHash_)

module type STATE = sig
  type _ t
  type 'var inp
  type 's out

  val inp_trans :
    ('obj, 'var inp) method_ ->
    ('var, 'k) constr ->
    unit NameUnify.t ->
    'k t ->
    'obj t

  val out_trans :
    ('obj, 'mt) method_ ->
    ('mt, 'k out) method_ ->
    unit NameUnify.t ->
    'k t ->
    'obj t

  val internal_choice : ('lr, 'l, 'r) disj -> 'l t -> 'r t -> 'lr t
  val merge : 's t -> 's t -> 's t
  val make_lazy : 's t lazy_t -> 's t
  val unit : unit t
end

module Global (State : STATE) = struct
  open State

  type all_unit = [ `cons of unit * 'a ] as 'a

  module Open = struct
    type _ t =
      | [] : all_unit t
      | ( :: ) : (unit, 'b, 'bb, 'cc) Hlist.idx * 'bb t -> 'cc t
  end

  type 't global =
    | Comm :
        ('a, 'b, 'c, 'd, 'e, 'f inp) role
        * ('g, 'e, 'h, 'c, 'b, 'i) role
        * ('i, 'a out, 'f, 'g) label
        * 'h global
        -> 'd global
    | ChoiceAt :
        ('a, 'b, 'c, 'd, 'e, 'f) role
        * ('b, 'g, 'h) disj
        * (('g, unit, 'i, 'c, 'j, 'k) role * 'i global)
        * (('h, unit, 'l, 'c, 'm, 'n) role * 'l global)
        -> 'd global
    | PartialFinish : 'b Open.t * 'b global -> 'b global
    | Finish : all_unit global

  module Sessions = Hlist.Make (State)
  include Sessions

  let rec merge_seq : type t. t seq -> t seq -> t seq =
   fun ls rs ->
    match (ls, rs) with
    | Sessions.[], _ -> []
    | _, [] -> []
    | l :: ls, r :: rs -> merge l r :: merge_seq ls rs

  let rec eval_ : type t. t global -> t seq =
   fun g ->
    match g with
    | Comm (ra, rb, lab, g) ->
        let key = NameUnify.newkey () in
        let g = eval_ g in
        let b = seq_get rb.role_index g in
        let g =
          seq_put rb.role_index g (inp_trans ra.role_label lab.var key b)
        in
        let a = seq_get ra.role_index g in
        let g =
          seq_put ra.role_index g (out_trans rb.role_label lab.obj key a)
        in
        g
    | ChoiceAt (ra, disj, (ra1, g1), (ra2, g2)) ->
        let g1 = eval_ g1 in
        let g2 = eval_ g2 in
        let a1 = seq_get ra1.role_index g1 and a2 = seq_get ra2.role_index g2 in
        let g1 = seq_put ra1.role_index g1 unit
        and g2 = seq_put ra2.role_index g2 unit in
        let g = merge_seq g1 g2 in
        let a = internal_choice disj a1 a2 in
        seq_put ra.role_index g a
    | PartialFinish (keep_idxs, keeps) ->
        let rec finish :
            type b. keep_idxs:b Open.t -> keeps:b seq lazy_t -> b seq =
         fun ~keep_idxs ~keeps ->
          match keep_idxs with
          | Open.[] -> Sessions.[]
          | idx :: rest_idxs ->
              let state =
                (* get the state of the involved role *)
                State.make_lazy (lazy (seq_get2 idx (Lazy.force keeps)))
              and rest =
                (* rest of the states *)
                lazy (seq_put2 idx (Lazy.force keeps) State.unit)
              in
              let states = finish ~keep_idxs:rest_idxs ~keeps:rest in
              (* put the state and return it *)
              seq_put idx states state
        in
        finish ~keep_idxs ~keeps:(lazy (eval_ keeps))
    | Finish -> []
end

module StateId = struct
  module StateHash = PolyAssoc (struct
    type 'a t = 'a
  end)

  type key_ex = KeyEx : 'a StateHash.key -> key_ex
  type 'a t = { id_head : 'a StateHash.key; id_tail : key_ex list }

  let union_sorted_lists (xs : 'a list) (ys : 'a list) =
    let rec loop aux xs ys =
      match (xs, ys) with
      | x :: xs, y :: ys ->
          if x = y then loop (x :: aux) xs ys
          else if x < y then loop (x :: aux) xs (y :: ys)
          else loop (y :: aux) (x :: xs) ys
      | [], ys -> List.rev aux @ ys
      | xs, [] -> List.rev aux @ xs
    in
    loop [] xs ys

  let general_union (type a b) (ks1 : a t) (ks2 : b t) : (a t, b t) Either.t =
    if KeyEx ks1.id_head < KeyEx ks2.id_head then
      Left
        {
          id_head = ks1.id_head;
          id_tail =
            union_sorted_lists ks1.id_tail (KeyEx ks2.id_head :: ks2.id_tail);
        }
    else if KeyEx ks2.id_head < KeyEx ks1.id_head then
      Right
        {
          id_head = ks2.id_head;
          id_tail =
            union_sorted_lists (KeyEx ks1.id_head :: ks1.id_tail) ks2.id_tail;
        }
    else
      Left
        {
          id_head = ks1.id_head;
          id_tail = union_sorted_lists ks1.id_tail ks2.id_tail;
        }

  let union (ks1 : 'a t) (ks2 : 'a t) : 'a t =
    match general_union ks1 ks2 with Left ks | Right ks -> ks

  let make () = { id_head = StateHash.newkey (); id_tail = [] }
end

module State = struct
  type _ t =
    | Deterministic : 'obj StateId.t * 'obj head -> 'obj t
    | Lazy : 'a t lazy_t -> 'a t
    | Epsilon : 'a t list -> 'a t
    | InternalChoice : 'lr StateId.t * ('lr, 'l, 'r) disj * 'l t * 'r t -> 'lr t

  and 't head = {
    head : 't;
    merge : 't -> 't -> 't;
    force_determinise : 't -> unit;
  }

  and _ out = Out : unit NameUnify.t * 's t lazy_t -> 's out
  and 'var inp = 'var inp0 list lazy_t
  and _ inp0 = Inp0 : ('var, 's) constr * unit NameUnify.t * 's t -> 'var inp0

  exception Unguarded of string

  let merge_heads hds =
    let merge_head : 'a. 'a head -> 'a head -> 'a head =
     fun dl dr ->
      let d' = dl.merge dl.head dr.head in
      { head = d'; merge = dl.merge; force_determinise = dl.force_determinise }
    in
    List.fold_left merge_head (List.hd hds) (List.tl hds)

  let try_cast_and_merge_heads constrA constrB contA contB =
    match Types.cast_if_constrs_are_same constrA constrB contB.head with
    | Some contB_body ->
        let head = contA.merge contA.head contB_body in
        Some { contA with head }
    | None -> None

  let force t =
    match t with
    | Deterministic (_, d) -> d.force_determinise d.head
    | _ -> failwith "Impossible: force: channel not determinised"

  module Determinise : sig
    val determinise_step : 's t -> 's StateId.t * 's head
  end = struct
    type 'a merged_or_backward_epsilon =
      ('a StateId.t * 'a head list, 'a t list) Either.t

    let rec mem_phys k = function
      | x :: xs -> k == x || mem_phys k xs
      | [] -> false

    let ret_merged x = Either.Left x
    and ret_backward_epsilon x = Either.Right x

    let fail_unguarded str = raise (Unguarded str)

    let rec determinise_step : 's. 's t -> 's StateId.t * 's head =
     fun st ->
      match epsilon_closure ~visited:[] st with
      | Left (sid, hds) -> (sid, merge_heads hds)
      | Right _ -> fail_unguarded "epsilon_closure: unguarded"

    and epsilon_closure :
        type a. visited:a t list -> a t -> a merged_or_backward_epsilon =
     fun ~visited st ->
      if mem_phys st visited then ret_backward_epsilon [ st ]
      else
        match st with
        | Deterministic (sid, v) -> ret_merged (sid, [ v ])
        | Epsilon sts ->
            (* epsilon transitions: compute epsilon-closure *)
            let hds, cycles =
              List.partition_map (epsilon_closure ~visited:(st :: visited)) sts
            in
            if List.length hds > 0 then
              (* concrete transitons found - return the merged state ==== *)
              let ids, hds = List.split hds in
              let id =
                List.fold_left StateId.union (List.hd ids) (List.tl ids)
              in
              ret_merged (id, List.concat hds)
            else
              (* all transitions are epsilon - verify guardedness ==== *)
              let cycles = List.concat cycles in
              let cycles =
                (* filter out epsilons pointing to myself *)
                List.filter (fun id -> mem_phys id visited) cycles
              in
              if List.length cycles > 0 then
                (* there're backward epsilons yet -- return it *)
                ret_backward_epsilon cycles
              else
                (* no backward epsilons anymore: unguarded recursion! *)
                fail_unguarded "epsilon_closure: unguarded"
        | InternalChoice (sid, disj, tl, tr) ->
            let _idl, dl = determinise_step tl
            and _idr, dr = determinise_step tr in
            let merge lr1 lr2 =
              (* merge splitted ones *)
              let l = dl.merge (disj.disj_splitL lr1) (disj.disj_splitL lr2) in
              let r = dr.merge (disj.disj_splitR lr1) (disj.disj_splitR lr2) in
              (* then type-concatenate it *)
              disj.disj_concat l r
            in
            let force_determinise lr =
              dl.force_determinise (disj.disj_splitL lr);
              dr.force_determinise (disj.disj_splitR lr)
            in
            let tlr = disj.disj_concat dl.head dr.head in
            ret_merged (sid, [ { head = tlr; merge; force_determinise } ])
        | Lazy t -> epsilon_closure ~visited (Lazy.force t)
  end

  module OutMerge = struct
    let real_merge l r =
      let idl, dl = Determinise.determinise_step l
      and idr, dr = Determinise.determinise_step r in
      Deterministic (StateId.union idl idr, merge_heads [ dl; dr ])

    let out_merge role lab s1 s2 =
      let (Out (name1, cont1)) = lab.call_obj @@ role.call_obj s1
      and (Out (name2, cont2)) = lab.call_obj @@ role.call_obj s2 in
      NameUnify.unify name1 name2;
      role.make_obj
      @@ lab.make_obj
      @@ Out (name1, lazy (real_merge (Lazy.force cont1) (Lazy.force cont2)))

    let out_force role lab s =
      let (Out (_, cont)) = lab.call_obj @@ role.call_obj s in
      force (Lazy.force cont)
  end

  module InpMerge = struct
    let try_real_merge_inp0 : type a. a inp0 -> a inp0 -> a inp0 option =
     fun l r ->
      match (l, r) with
      | Inp0 (constr1, name1, cont1), Inp0 (constr2, name2, cont2) -> (
          (* check if constr1 = constr2 and merge cont1 with cont2 using
             "try_cast_and_merge_heads" above. To do so:
               (1) We need to step-determinise the continuation, as required by
                   the function
               (2) Compute the new state id via 'generalised union'
                   (StateId.general_union)
          *)
          (* (1) extract the channel objects ==== *)
          let state_id1, cont1 = Determinise.determinise_step cont1 in
          let state_id2, cont2 = Determinise.determinise_step cont2 in
          let make_inp constr state_id cont =
            NameUnify.unify name1 name2;
            Inp0 (constr, name1, Deterministic (state_id, cont))
          in
          (* (2) compute the new state id ==== *)
          match StateId.general_union state_id1 state_id2 with
          | Left state_id ->
              try_cast_and_merge_heads constr1 constr2 cont1 cont2
              |> Option.map (make_inp constr1 state_id)
          | Right state_id ->
              try_cast_and_merge_heads constr2 constr1 cont2 cont1
              |> Option.map (make_inp constr2 state_id))

    let rec real_merge_inp_inp0 : type a. a inp0 list -> a inp0 -> a inp0 list =
     fun inp inp0 ->
      match inp with
      | i0 :: inp -> (
          match try_real_merge_inp0 i0 inp0 with
          | Some i0 -> i0 :: inp
          | None -> i0 :: real_merge_inp_inp0 inp inp0)
      | [] -> [ inp0 ]

    let real_merge_inp s1 s2 = List.fold_left real_merge_inp_inp0 s1 s2

    let inp_merge role s1 s2 =
      role.make_obj
      @@ lazy
           (let s1 = role.call_obj s1 and s2 = role.call_obj s2 in
            real_merge_inp (Lazy.force s1) (Lazy.force s2))

    let inp_force role s =
      let s = role.call_obj s in
      List.iter (fun (Inp0 (_, _, s)) -> force s) (Lazy.force s)
  end

  let out_trans role lab name s =
    Deterministic
      ( StateId.make (),
        {
          head = role.make_obj @@ lab.make_obj @@ Out (name, Lazy.from_val s);
          merge = OutMerge.out_merge role lab;
          force_determinise = OutMerge.out_force role lab;
        } )

  let inp_trans role constr name s =
    Deterministic
      ( StateId.make (),
        {
          head = role.make_obj @@ lazy [ Inp0 (constr, name, s) ];
          merge = InpMerge.inp_merge role;
          force_determinise = InpMerge.inp_force role;
        } )

  let internal_choice disj l r = InternalChoice (StateId.make (), disj, l, r)
  let merge l r = Epsilon [ l; r ]
  let make_lazy t = Lazy t

  let unit =
    Deterministic
      ( StateId.make (),
        {
          head = ();
          merge = (fun _ _ -> ());
          force_determinise = (fun _ -> ());
        } )

  let eval t =
    let _, d = Determinise.determinise_step t in
    d.force_determinise d.head;
    d.head
end

module State0 = struct
  type _ t =
    | Epsilon : 'a t list -> 'a t
    | Deterministic : 'obj StateId.t * 'obj trans -> 'obj t
    | InternalChoice : 'lr StateId.t * ('lr, 'l, 'r) disj * 'l t * 'r t -> 'lr t
    | Lazy : 'a t lazy_t -> 'a t

  and _ trans =
    | OutTrans :
        ('obj, 'mt) method_ * ('mt, 'k out) method_ * 'k out
        -> 'obj trans
    | InpTrans : ('obj, 'var inp) method_ * 'var inp -> 'obj trans
    | End : unit trans

  and 'var inp = 'var inp0 list
  and _ inp0 = Inp0 : ('var, 's) constr * unit NameUnify.t * 's t -> 'var inp0
  and _ out = Out : unit NameUnify.t * 's t -> 's out

  let out_trans role lab name s =
    Deterministic (StateId.make (), OutTrans (role, lab, Out (name, s)))

  let inp_trans role constr name s =
    Deterministic (StateId.make (), InpTrans (role, [ Inp0 (constr, name, s) ]))

  let internal_choice disj l r = InternalChoice (StateId.make (), disj, l, r)
  let merge l r = Epsilon [ l; r ]
  let make_lazy t = Lazy t
  let unit = Deterministic (StateId.make (), End)
end
