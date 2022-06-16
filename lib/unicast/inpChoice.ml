module Context = State.Context

type _ t = Choice : ('var, 's) Rows.constr * 's LinState.t -> 'var t

exception Not_matching

let cast constr1 constr2 a =
  match Rows.cast_if_constrs_are_same constr1 constr2 a with
  | Some a -> a
  | None -> raise Not_matching

let cast_gen constr1 constr2 cont2 = Lin.map_gen (cast constr1 constr2) cont2

let merge_st_if_same (type x) constr1 constr2 ctx (t1 : _ State.t)
    (t2 : _ State.t) =
  let (module D : State.Op with type a = x Lin.gen) = t1.st_op
  and cont1, cont2 = (t1.st, t2.st) in
  let cont2 = cast_gen constr1 constr2 cont2 in
  let cont = D.merge ctx cont1 cont2 in
  State.{ st = cont; st_op = (module D) }

let merge_pow_if_same constr1 constr2 ctx (t1 : _ LinState.t)
    (t2 : _ LinState.t) =
  (* check if constr1 = constr2 and then merge t1 with t2.
      To compare the constructors, we need to get the raw/final form of the
      session object (the type 'a in the type 'a LinState.t), as
      `Rows.cast_if_constrs_are_same` requires so. Thus (1) we determinise the
      continuation here (using the merging context ctx).

      And if the constructors are same, we need the new union'ed state id as
      well. The problem is that there are no means to cast types of state ids --
      `Rows.cast_if_constrs_are_same` cannot cast type 'a state_id to 'b
      state_id even when it can cast 'a to 'b. Instead of casting them, (2) we
      compute the union of ids via 'generalised union'
      (Context.make_union_keys_general) which does not distinguish types.
  *)
  (* (1) determinise the continuations ==== *)
  let state_id1, (lazy t1) = PowState.determinise_core ctx t1 in
  let state_id2, (lazy t2) = PowState.determinise_core ctx t2 in
  (* (2) compute the new state id ==== *)
  match Context.union_keys_general state_id1 state_id2 with
  | Left state_id1 ->
      (* (3) try to cast and merge them ==== *)
      let t = merge_st_if_same constr1 constr2 ctx t1 t2 in
      Either.Left (PowState.make_raw state_id1 (Lazy.from_val t))
  | Right state_id2 ->
      (* (3) try to cast and merge them ==== *)
      let t = merge_st_if_same constr2 constr1 ctx t2 t1 in
      Right (PowState.make_raw state_id2 (Lazy.from_val t))

let merge_choice : type a. State.context -> a t -> a t -> a t option =
 fun ctx e1 e2 ->
  let Choice (constr1, t1), Choice (constr2, t2) = (e1, e2) in
  match merge_pow_if_same constr1 constr2 ctx t1 t2 with
  | Left t1 -> Some (Choice (constr1, t1))
  | Right t2 -> Some (Choice (constr2, t2))
  | exception Not_matching -> None

let merge ctx exts1 exts2 =
  let rec merge_loop ctx exts ext_one =
    match exts with
    | ext :: exts -> (
        match merge_choice ctx ext ext_one with
        | Some e ->
            (* found *)
            e :: exts
        | None -> ext :: merge_loop ctx exts ext_one)
    | [] ->
        (* not found *)
        [ ext_one ]
  in
  List.fold_left (merge_loop ctx) exts1 exts2

let to_string ctx (Choice (constr, cont)) =
  constr.constr_name ^ "." ^ PowState.to_string ctx cont

let force ctx (Choice (_, s)) = PowState.force ctx s
let make constr s = Choice (constr, s)

let determinise ctx (Choice (constr, cont)) =
  Choice (constr, PowState.determinise ctx cont)

let match_item (Choice (var, cont)) =
  ( Btype.hash_variant var.constr_name,
    lazy (var.make_var @@ Lin.fresh @@ PowState.ensure_determinised cont) )
