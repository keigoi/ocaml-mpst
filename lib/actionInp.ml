module Context = State.Context
open Rows

type tag = int

type _ extchoice_item =
  | ExternalChoiceItem :
      ('var, 's) Rows.constr * 's State.t
      -> 'var extchoice_item

type 'var inp_ = tag DynChan.name * 'var extchoice_item list
type 'var inp = 'var inp_ lazy_t

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

let determinise_item ctx (ExternalChoiceItem (constr, cont)) =
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
          e :: List.map (determinise_item ctx) exts
      | None ->
          determinise_item ctx ext :: merge_item_many_with_one ctx exts ext_one)
  | [] ->
      (* not found *)
      [ determinise_item ctx ext_one ]

let merge_item_many_with_many ctx exts1 exts2 =
  List.fold_left (merge_item_many_with_one ctx) exts1 exts2

let merge role ctx s1 s2 =
  role.make_obj
  @@ lazy
       begin
         let name1, exts1 = Lazy.force @@ role.call_obj s1
         and name2, exts2 = Lazy.force @@ role.call_obj s2 in
         DynChan.unify name1 name2;
         (name1, merge_item_many_with_many ctx exts1 exts2)
       end

let det_inp_ops (type a) (role : (a, _) method_) =
  let module DetInp = struct
    type nonrec a = a

    let determinise ctx = function
      | [ s ] ->
          role.make_obj
            (lazy
              begin
                let name, exts = Lazy.force (role.call_obj s) in
                (name, List.map (determinise_item ctx) exts)
              end)
      | s :: ss -> List.fold_left (merge role ctx) s ss
      | [] -> failwith "impossible: inp_determinise"

    let force ctx s =
      let name, extcs = Lazy.force @@ role.call_obj s in
      ignore (DynChan.finalise name);
      extcs
      |> List.iter (fun (ExternalChoiceItem (_, s)) -> State.force_core ctx s)

    let to_string ctx s =
      role.method_name
      ^ "?{"
      ^ (let s = role.call_obj s in
         if Lazy.is_val s then
           String.concat ","
             (List.map
                (fun (ExternalChoiceItem (constr, cont)) ->
                  constr.constr_name ^ "." ^ State.to_string_core ctx cont)
                (snd (Lazy.force s)))
         else "<lazy_inp>")
      ^ "}"
  end in
  (module DetInp : State.DetState with type a = a)

let inp role constr name s =
  State.make_deterministic (Context.new_key ())
  @@ Lazy.from_val
       State.
         {
           det_state =
             role.make_obj
               (Lazy.from_val (name, [ ExternalChoiceItem (constr, s) ]));
           det_ops = det_inp_ops role;
         }

let branch (inp : _ inp) =
  let make_event (ExternalChoiceItem (var, cont)) =
    let cont = State.ensure_determinised cont in
    (Btype.hash_variant var.constr_name, var.make_var cont)
  in
  let name, items = Lazy.force inp in
  items
  |> List.map make_event
  |> List.assoc (DynChan.receive (DynChan.finalise name))
