open Rows
module StateHash = State.StateHash

type tag = int

module Make (Name : S.Name) = struct
  type _ out = Out : string * tag Name.name * 's State.t lazy_t -> 's out

  let merge_cont ctx l r =
    let idl, dl = State.determinise_core ctx l
    and idr, dr = State.determinise_core ctx r in
    let state_id = StateHash.make_union_keys idl idr in
    State.deterministic state_id
      (State.determinise_list ctx state_id [ dl; dr ])

  let determinise_one role lab ctx s =
    let (Out (tag, name, cont)) = lab.call_obj (role.call_obj s) in
    role.make_obj
    @@ lab.make_obj
    @@ Out
         ( tag,
           name,
           lazy
             (let state_id, d = State.determinise_core ctx (Lazy.force cont) in
              State.deterministic state_id d) )

  let merge role lab ctx s1 s2 =
    let (Out (tag1, name1, cont1)) = lab.call_obj (role.call_obj s1)
    and (Out (tag2, name2, cont2)) = lab.call_obj (role.call_obj s2) in
    assert (tag1 = tag2);
    Name.unify name1 name2;
    role.make_obj
    @@ lab.make_obj
    @@ Out
         ( tag1,
           name1,
           lazy (merge_cont ctx (Lazy.force cont1) (Lazy.force cont2)) )

  let determinise_list role lab ctx = function
    | [ s ] -> determinise_one role lab ctx s
    | t :: ts -> List.fold_left (merge role lab ctx) t ts
    | [] -> failwith "out_determinise: impossible"

  let force_traverse role lab ctx s =
    let (Out (_, name, cont)) = lab.call_obj (role.call_obj s) in
    ignore (Name.finalise name);
    State.force_traverse ctx (Lazy.force cont)

  let to_string role lab ctx s =
    let (Out (_, _, cont)) = lab.call_obj (role.call_obj s) in
    role.method_name
    ^ "!"
    ^ lab.method_name
    ^ "."
    ^
    if Lazy.is_val cont then
      let cont = Lazy.force cont in
      State.to_string ctx cont
    else "<lazy_out_cont>"

  let out role lab name s =
    State.deterministic (StateHash.make_key ())
    @@ Lazy.from_val
         {
           State.body =
             role.make_obj
               (lab.make_obj (Out (lab.method_name, name, Lazy.from_val s)));
           determinise_list = determinise_list role lab;
           force_traverse = force_traverse role lab;
           to_string = to_string role lab;
         }
end