module Context = State.Context

type _ t = Choice : ('var, 's) Rows.constr * 's State.t -> 'var t

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
  let make_ constr state_id cont =
    Choice (constr, State.make_deterministic state_id cont)
  in
  (* (2) compute the new state id ==== *)
  match Context.union_keys_general state_id1 state_id2 with
  | Left state_id ->
      State.try_merge_det ctx state_id constr1 constr2 cont1 cont2
      |> Option.map (make_ constr1 state_id)
  | Right state_id ->
      State.try_merge_det ctx state_id constr2 constr1 cont2 cont1
      |> Option.map (make_ constr2 state_id)

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
  let cont = State.ensure_determinised cont in
  (Btype.hash_variant var.constr_name, var.make_var cont)
