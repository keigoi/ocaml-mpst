module Context = State.Context

type _ extchoice_item =
  | ExternalChoiceItem :
      ('var, 's) Rows.constr * 's State.t
      -> 'var extchoice_item

let try_merge_item :
    type a.
    State.context ->
    a extchoice_item ->
    a extchoice_item ->
    a extchoice_item option =
 fun ctx e1 e2 ->
  let ExternalChoiceItem (constr1, cont1), ExternalChoiceItem (constr2, cont2) =
    (e1, e2)
  in
  (* check if constr1 = constr2 and then merge cont1 with cont2 using
     State.try_merge_det. To do so:
       (1) We need to determinise the continuation
       (2) Compute the new state id via 'generalised union'
           (Context.make_union_keys_general)
  *)
  (* (1) extract the continuations ==== *)
  let state_id1, cont1 = State.determinise_core_ ctx cont1 in
  let state_id2, cont2 = State.determinise_core_ ctx cont2 in
  let make_ constr state_id cont =
    ExternalChoiceItem (constr, State.make_deterministic state_id cont)
  in
  (* (2) compute the new state id ==== *)
  match Context.union_keys_general state_id1 state_id2 with
  | Left state_id ->
      State.try_merge_det ctx state_id constr1 constr2 cont1 cont2
      |> Option.map (make_ constr1 state_id)
  | Right state_id ->
      State.try_merge_det ctx state_id constr2 constr1 cont2 cont1
      |> Option.map (make_ constr2 state_id)

let determinise ctx (ExternalChoiceItem (constr, cont)) =
  ExternalChoiceItem (constr, State.determinise_core ctx cont)

let rec merge_item_many_with_one :
    type a.
    State.context ->
    a extchoice_item list ->
    a extchoice_item ->
    a extchoice_item list =
 fun ctx exts ext_one ->
  match exts with
  | ext :: exts -> (
      match try_merge_item ctx ext ext_one with
      | Some e ->
          (* found *)
          e :: List.map (determinise ctx) exts
      | None -> determinise ctx ext :: merge_item_many_with_one ctx exts ext_one
      )
  | [] ->
      (* not found *)
      [ determinise ctx ext_one ]

let make constr s = ExternalChoiceItem (constr, s)

let merge_items ctx exts1 exts2 =
  List.fold_left (merge_item_many_with_one ctx) exts1 exts2

let to_string ctx (ExternalChoiceItem (constr, cont)) =
  constr.constr_name ^ "." ^ State.to_string_core ctx cont

let force ctx (ExternalChoiceItem (_, s)) = State.force_core ctx s

let match_item (ExternalChoiceItem (var, cont)) =
  let cont = State.ensure_determinised cont in
  (Btype.hash_variant var.constr_name, var.make_var cont)
