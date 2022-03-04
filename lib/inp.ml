open Rows
open State

type tag = int

module Make (Name : S.Name) = struct
  type _ extchoice_item =
    | ExternalChoiceItem : ('var, 's) constr * 's t -> 'var extchoice_item

  type 'var inp_ = tag Name.name * 'var extchoice_item list
  type 'var inp = 'var inp_ lazy_t

  let try_real_merge_extchoice_item :
      type a.
      Head.context ->
      a extchoice_item ->
      a extchoice_item ->
      a extchoice_item option =
   fun ctx l r ->
    let ExternalChoiceItem (constr1, cont1), ExternalChoiceItem (constr2, cont2)
        =
      (l, r)
    in
    (* check if constr1 = constr2 and then merge cont1 with cont2 using
       "try_cast_and_merge_heads" above. To do so:
         (1) We need to step-determinise the continuation, as required by
             the function
         (2) Compute the new state id via 'generalised union'
             (StateId.general_union)
    *)
    (* (1) extract the channel objects ==== *)
    let state_id1, cont1 = Determinise.determinise ctx cont1 in
    let state_id2, cont2 = Determinise.determinise ctx cont2 in
    let make_ constr state_id cont =
      ExternalChoiceItem (constr, deterministic state_id cont)
    in
    (* (2) compute the new state id ==== *)
    match Head.general_union_keys state_id1 state_id2 with
    | Left state_id ->
        try_cast_then_merge_heads ctx state_id constr1 constr2 cont1 cont2
        |> Option.map (make_ constr1 state_id)
    | Right state_id ->
        try_cast_then_merge_heads ctx state_id constr2 constr1 cont2 cont1
        |> Option.map (make_ constr2 state_id)

  let determinise_extchoice_item ctx (ExternalChoiceItem (constr, cont)) =
    let state_id, d = Determinise.determinise ctx cont in
    ExternalChoiceItem (constr, deterministic state_id d)

  let rec real_inp_merge_one :
      type a.
      Head.context ->
      a extchoice_item list ->
      a extchoice_item ->
      a extchoice_item list =
   fun ctx inp extc_item ->
    match inp with
    | e :: inp -> (
        match try_real_merge_extchoice_item ctx e extc_item with
        | Some e -> e :: List.map (determinise_extchoice_item ctx) inp
        | None ->
            determinise_extchoice_item ctx e
            :: real_inp_merge_one ctx inp extc_item)
    | [] -> [ determinise_extchoice_item ctx extc_item ]

  let real_inp_merge ctx s1 s2 = List.fold_left (real_inp_merge_one ctx) s1 s2

  let inp_merge role ctx s1 s2 =
    role.make_obj
    @@ lazy
         begin
           let s1 = role.call_obj s1 and s2 = role.call_obj s2 in
           let name1, s1 = Lazy.force s1 and name2, s2 = Lazy.force s2 in
           Name.unify name1 name2;
           (name1, real_inp_merge ctx s1 s2)
         end

  let inp_determinise role ctx = function
    | [ s ] ->
        role.make_obj
          (lazy
            begin
              let name, exts = Lazy.force (role.call_obj s) in
              (name, List.map (determinise_extchoice_item ctx) exts)
            end)
    | s :: ss -> List.fold_left (inp_merge role ctx) s ss
    | [] -> failwith "impossible: inp_determinise"

  let inp_force role ctx s =
    let s = role.call_obj s in
    let name, extcs = Lazy.force s in
    ignore (Name.finalise name);
    extcs
    |> List.iter (fun (ExternalChoiceItem (_, s)) -> force_determinised ctx s)

  let inp_to_string role ctx s =
    role.method_name
    ^ "?{"
    ^ (let s = role.call_obj s in
       if Lazy.is_val s then
         String.concat ","
           (List.map
              (fun (ExternalChoiceItem (constr, cont)) ->
                constr.constr_name ^ "." ^ to_string ctx cont)
              (snd @@ Lazy.force s))
       else "<lazy_inp>")
    ^ "}"
end