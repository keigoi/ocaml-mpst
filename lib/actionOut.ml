open Rows
module StateHash = State.StateHash

type tag = int
type _ out = Out : string * tag DynChan.name * 's State.t lazy_t -> 's out

let merge_cont ctx l r =
  let idl, dl = State.determinise_core ctx l
  and idr, dr = State.determinise_core ctx r in
  let state_id = StateHash.union_keys idl idr in
  State.make_deterministic state_id
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
            State.make_deterministic state_id d) )

let merge role lab ctx s1 s2 =
  let (Out (tag1, name1, cont1)) = lab.call_obj (role.call_obj s1)
  and (Out (tag2, name2, cont2)) = lab.call_obj (role.call_obj s2) in
  assert (tag1 = tag2);
  DynChan.unify name1 name2;
  role.make_obj
  @@ lab.make_obj
  @@ Out
       (tag1, name1, lazy (merge_cont ctx (Lazy.force cont1) (Lazy.force cont2)))

let det_state_ops (type a) (role : (a, _) method_) lab =
  let module M = struct
    type nonrec a = a

    let determinise ctx = function
      | [ s ] -> determinise_one role lab ctx s
      | t :: ts -> List.fold_left (merge role lab ctx) t ts
      | [] -> failwith "out_determinise: impossible"

    let force ctx s =
      let (Out (_, name, cont)) = lab.call_obj (role.call_obj s) in
      ignore (DynChan.finalise name);
      State.force ctx (Lazy.force cont)

    let to_string ctx s =
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
  end in
  (module M : State.DetState with type a = a)

let out role lab name s =
  State.make_deterministic (StateHash.new_key ())
  @@ Lazy.from_val
       {
         State.det_state =
           role.make_obj
             (lab.make_obj (Out (lab.method_name, name, Lazy.from_val s)));
         det_ops = det_state_ops role lab;
       }

let select (Out (labname, name, cont)) =
  let tag = Btype.hash_variant labname in
  let cont = State.ensure_determinised (Lazy.force cont) in
  DynChan.send (DynChan.finalise name) tag;
  cont
