module Context = State.Context

type _ t = Choice : ('var, 's) Rows.constr * 's LinState.t -> 'var t

exception Not_matching

let try_cast_exn constr1 constr2 a =
  match Rows.cast_if_constrs_are_same constr1 constr2 a with
  | Some a -> a
  | None -> raise Not_matching

let cast_and_merge (type x) (module D : State.DetState with type a = x Lin.gen)
    ctx state_id constr1 constr2 cont1 cont2 =
  let cont2 = Lin.map_gen (try_cast_exn constr1 constr2) cont2 in
  let cont =
    Lazy.from_val
    @@ State.{ det_state = D.merge ctx cont1 cont2; det_ops = (module D) }
  in
  Choice (constr1, State.make_deterministic state_id cont)

let try_merge_item : type a. State.context -> a t -> a t -> a t option =
 fun ctx e1 e2 ->
  let Choice (constr1, cont1), Choice (constr2, cont2) = (e1, e2) in
  (* check if constr1 = constr2 and then merge cont1 with cont2 using
     State.try_merge_det. To do so:
       (1) We need to determinise the continuation (thus needs the merging context as well!)
       (2) Compute the new state id via 'generalised union'
           (Context.make_union_keys_general)
  *)
  (* (1) extract the continuations ==== *)
  let state_id1, cont1 = State.determinise_core_ ctx cont1 in
  let state_id2, cont2 = State.determinise_core_ ctx cont2 in
  let State.{ det_ops = (module D1); det_state = cont1 } = Lazy.force cont1 in
  let State.{ det_ops = (module D2); det_state = cont2 } = Lazy.force cont2 in
  (* (2) compute the new state id ==== *)
  try
    let t =
      (* (3) cast and merge them ==== *)
      match Context.union_keys_general state_id1 state_id2 with
      | Left state_id ->
          cast_and_merge (module D1) ctx state_id constr1 constr2 cont1 cont2
      | Right state_id ->
          cast_and_merge (module D2) ctx state_id constr2 constr1 cont2 cont1
    in
    Some t
  with Not_matching -> None

let merge ctx exts1 exts2 =
  let rec merge_accum ctx exts ext_one =
    match exts with
    | ext :: exts -> (
        match try_merge_item ctx ext ext_one with
        | Some e ->
            (* found *)
            e :: exts
        | None -> ext :: merge_accum ctx exts ext_one)
    | [] ->
        (* not found *)
        [ ext_one ]
  in
  List.fold_left (merge_accum ctx) exts1 exts2

let to_string ctx (Choice (constr, cont)) =
  constr.constr_name ^ "." ^ State.to_string_core ctx cont

let force ctx (Choice (_, s)) = State.force_core ctx s
let make constr s = Choice (constr, s)

let determinise ctx (Choice (constr, cont)) =
  Choice (constr, State.determinise_core ctx cont)

let match_item (Choice (var, cont)) =
  ( Btype.hash_variant var.constr_name,
    lazy (var.make_var @@ Lin.fresh @@ State.ensure_determinised cont) )
