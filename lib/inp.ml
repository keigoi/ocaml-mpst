open Rows
module StateHash = State.StateHash

type tag = int

module Make (Name : S.Name) = struct
  type _ extchoice_item =
    | ExternalChoiceItem : ('var, 's) constr * 's State.t -> 'var extchoice_item

  type 'var inp_ = tag Name.name * 'var extchoice_item list
  type 'var inp = 'var inp_ lazy_t

  let try_merge_item :
      type a.
      State.context ->
      a extchoice_item ->
      a extchoice_item ->
      a extchoice_item option =
   fun ctx e1 e2 ->
    let ExternalChoiceItem (constr1, cont1), ExternalChoiceItem (constr2, cont2)
        =
      (e1, e2)
    in
    (* check if constr1 = constr2 and then merge cont1 with cont2 using
       State.try_cast_and_merge_determinise. To do so:
         (1) We need to step-determinise the continuation, as required by
             the function
         (2) Compute the new state id via 'generalised union'
             (StateId.general_union)
    *)
    (* (1) extract the channel objects ==== *)
    let state_id1, cont1 = State.determinise_core ctx cont1 in
    let state_id2, cont2 = State.determinise_core ctx cont2 in
    let make_ constr state_id cont =
      ExternalChoiceItem (constr, State.deterministic state_id cont)
    in
    (* (2) compute the new state id ==== *)
    match StateHash.make_union_keys_general state_id1 state_id2 with
    | Left state_id ->
        State.try_cast_and_merge_determinise ctx state_id constr1 constr2 cont1
          cont2
        |> Option.map (make_ constr1 state_id)
    | Right state_id ->
        State.try_cast_and_merge_determinise ctx state_id constr2 constr1 cont2
          cont1
        |> Option.map (make_ constr2 state_id)

  let determinise_ext ctx (ExternalChoiceItem (constr, cont)) =
    let state_id, d = State.determinise_core ctx cont in
    ExternalChoiceItem (constr, State.deterministic state_id d)

  let rec merge_item_many_with_one :
      type a.
      State.context ->
      a extchoice_item list ->
      a extchoice_item ->
      a extchoice_item list =
   fun ctx inp extc_item ->
    match inp with
    | e :: inp -> (
        match try_merge_item ctx e extc_item with
        | Some e ->
            (* found *)
            e :: List.map (determinise_ext ctx) inp
        | None ->
            determinise_ext ctx e :: merge_item_many_with_one ctx inp extc_item)
    | [] ->
        (* not found *)
        [ determinise_ext ctx extc_item ]

  let merge_item_many_with_many ctx s1 s2 =
    List.fold_left (merge_item_many_with_one ctx) s1 s2

  let merge role ctx s1 s2 =
    role.make_obj
    @@ lazy
         begin
           let s1 = role.call_obj s1 and s2 = role.call_obj s2 in
           let name1, s1 = Lazy.force s1 and name2, s2 = Lazy.force s2 in
           Name.unify name1 name2;
           (name1, merge_item_many_with_many ctx s1 s2)
         end

  let determinise_list role ctx = function
    | [ s ] ->
        role.make_obj
          (lazy
            begin
              let name, exts = Lazy.force (role.call_obj s) in
              (name, List.map (determinise_ext ctx) exts)
            end)
    | s :: ss -> List.fold_left (merge role ctx) s ss
    | [] -> failwith "impossible: inp_determinise"

  let force_traverse role ctx s =
    let s = role.call_obj s in
    let name, extcs = Lazy.force s in
    ignore (Name.finalise name);
    extcs
    |> List.iter (fun (ExternalChoiceItem (_, s)) -> State.force_traverse ctx s)

  let to_string role ctx s =
    role.method_name
    ^ "?{"
    ^ (let s = role.call_obj s in
       if Lazy.is_val s then
         String.concat ","
           (List.map
              (fun (ExternalChoiceItem (constr, cont)) ->
                constr.constr_name ^ "." ^ State.to_string ctx cont)
              (snd (Lazy.force s)))
       else "<lazy_inp>")
    ^ "}"

  let inp role constr name s =
    State.deterministic (StateHash.make_key ())
    @@ Lazy.from_val
         {
           State.body =
             role.make_obj
               (Lazy.from_val (name, [ ExternalChoiceItem (constr, s) ]));
           determinise_list = determinise_list role;
           force_traverse = force_traverse role;
           to_string = to_string role;
         }
end
