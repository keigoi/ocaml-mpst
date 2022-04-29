open Rows
module Context = State.Context

type tag = int

type _ scatter_ =
  | Scatter :
      string * tag DynChan.name list * 's LinState.t lazy_t
      -> 's scatter_

type 'a scatter = 'a scatter_ Lin.lin

let merge_cont ctx l r =
  let idl, dl = State.determinise_core_ ctx l
  and idr, dr = State.determinise_core_ ctx r in
  let state_id = Context.union_keys idl idr in
  State.make_deterministic state_id (State.merge_det ctx state_id dl dr)

let scatter_ops (type b) role lab =
  let module DetScatter = struct
    type nonrec a = b scatter_

    let determinise ctx (Scatter (tag, name, cont)) =
      Scatter (tag, name, lazy (State.determinise_core ctx (Lazy.force cont)))

    let merge ctx (Scatter (tag1, names1, cont1))
        (Scatter (tag2, names2, cont2)) =
      assert (tag1 = tag2);
      List.iter2 DynChan.unify names1 names2;
      Scatter
        ( tag1,
          names1,
          lazy (merge_cont ctx (Lazy.force cont1) (Lazy.force cont2)) )

    let force ctx (Scatter (_, names, cont)) =
      ignore (List.map DynChan.finalise names);
      State.force_core ctx (Lazy.force cont)

    let to_string ctx (Scatter (_, _, cont)) =
      "."
      ^
      if Lazy.is_val cont then
        let cont = Lazy.force cont in
        State.to_string_core ctx cont
      else "<lazy_out_cont>"
  end in
  LinState.det_gen_ops
  @@ State.det_wrap_obj role
  @@ State.det_wrap_obj lab
  @@ LinState.det_lin_ops
       (module DetScatter : State.DetState with type a = b scatter_)

let out_state role lab name s =
  Lin.map_gen (fun s -> role.make_obj @@ lab.make_obj s)
  @@ Lin.declare (Scatter (lab.method_name, name, Lazy.from_val s))

let make_scatter role lab name s =
  State.make_deterministic (Context.new_key ())
  @@ Lazy.from_val
       State.
         {
           det_state = out_state role lab name s;
           det_ops = scatter_ops role lab;
         }

let scatter s =
  let (Scatter (labname, names, cont)) = Lin.use s in
  let tag = Btype.hash_variant labname in
  let cont = State.ensure_determinised (Lazy.force cont) in
  List.iter (fun name -> DynChan.send (DynChan.finalise name) tag) names;
  Lin.fresh cont
