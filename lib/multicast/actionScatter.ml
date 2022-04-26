open Rows
module Context = State.Context

type tag = int

type _ scatter =
  | Scatter : string * tag DynChan.name list * 's State.t lazy_t -> 's scatter

let merge_cont ctx l r =
  let idl, dl = State.determinise_core_ ctx l
  and idr, dr = State.determinise_core_ ctx r in
  let state_id = Context.union_keys idl idr in
  State.make_deterministic state_id (State.merge_det ctx state_id dl dr)

let determinise_one role lab ctx s =
  let (Scatter (tag, name, cont)) = lab.call_obj (role.call_obj s) in
  role.make_obj
  @@ lab.make_obj
  @@ Scatter (tag, name, lazy (State.determinise_core ctx (Lazy.force cont)))

let scatter_ops (type b) (role : (b, _) method_) lab =
  let module DetScatter = struct
    type nonrec a = b

    let determinise ctx s = determinise_one role lab ctx s

    let merge ctx s1 s2 =
      let (Scatter (tag1, names1, cont1)) = lab.call_obj (role.call_obj s1)
      and (Scatter (tag2, names2, cont2)) = lab.call_obj (role.call_obj s2) in
      assert (tag1 = tag2);
      List.iter2 DynChan.unify names1 names2;
      role.make_obj
      @@ lab.make_obj
      @@ Scatter
           ( tag1,
             names1,
             lazy (merge_cont ctx (Lazy.force cont1) (Lazy.force cont2)) )

    let force ctx s =
      let (Scatter (_, names, cont)) = lab.call_obj (role.call_obj s) in
      ignore (List.map DynChan.finalise names);
      State.force_core ctx (Lazy.force cont)

    let to_string ctx s =
      let (Scatter (_, _, cont)) = lab.call_obj (role.call_obj s) in
      role.method_name
      ^ "!!"
      ^ lab.method_name
      ^ "."
      ^
      if Lazy.is_val cont then
        let cont = Lazy.force cont in
        State.to_string_core ctx cont
      else "<lazy_out_cont>"
  end in
  (module DetScatter : State.DetState with type a = b)

let out_state role lab name s =
  role.make_obj
    (lab.make_obj (Scatter (lab.method_name, name, Lazy.from_val s)))

let make_scatter role lab name s =
  State.make_deterministic (Context.new_key ())
  @@ Lazy.from_val
       State.
         {
           det_state = out_state role lab name s;
           det_ops = scatter_ops role lab;
         }

let scatter (Scatter (labname, names, cont)) =
  let tag = Btype.hash_variant labname in
  let cont = State.ensure_determinised (Lazy.force cont) in
  List.iter (fun name -> DynChan.send (DynChan.finalise name) tag) names;
  cont
