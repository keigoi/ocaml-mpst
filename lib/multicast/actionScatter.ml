open Rows
module Context = State.Context

type tag = int

type _ scatter_ =
  | Scatter :
      string * tag DynChan.name list * 's LinState.t lazy_t
      -> 's scatter_

type 'a scatter = 'a scatter_ Lin.lin

let scatter_op0 (type b) : b scatter_ State.op =
  let module M = struct
    type nonrec a = b scatter_

    let determinise ctx (Scatter (tag, name, cont)) =
      Scatter (tag, name, lazy (PowState.determinise ctx (Lazy.force cont)))

    let merge ctx (Scatter (tag1, names1, cont1))
        (Scatter (tag2, names2, cont2)) =
      assert (tag1 = tag2);
      List.iter2 DynChan.unify names1 names2;
      Scatter
        ( tag1,
          names1,
          lazy (PowState.merge ctx (Lazy.force cont1) (Lazy.force cont2)) )

    let force ctx (Scatter (_, names, cont)) =
      ignore (List.map DynChan.finalise names);
      PowState.force ctx (Lazy.force cont)

    let to_string ctx (Scatter (_, _, cont)) =
      "."
      ^
      if Lazy.is_val cont then
        let cont = Lazy.force cont in
        PowState.to_string ctx cont
      else "<lazy_out_cont>"
  end in
  (module M)

let scatter_op role lab =
  LinState.gen_op
  @@ State.obj_op role
  @@ State.obj_op lab
  @@ LinState.lin_op
  @@ scatter_op0

let make_scatter role lab name s =
  let st =
    Lin.map_gen (fun t -> role.make_obj @@ lab.make_obj t)
    @@ Lin.declare
    @@ Scatter (lab.method_name, name, Lazy.from_val s)
  in
  PowState.make (scatter_op role lab) st

let scatter s =
  let (Scatter (labname, names, cont)) = Lin.use s in
  let tag = Btype.hash_variant labname in
  let cont = PowState.ensure_determinised (Lazy.force cont) in
  List.iter (fun name -> DynChan.send (DynChan.finalise name) tag) names;
  Lin.fresh cont
