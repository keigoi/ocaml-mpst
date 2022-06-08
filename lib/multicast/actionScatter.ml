open Rows
module Context = State.Context

type tag = int

type _ scatter_ =
  | Scatter :
      string * tag DynChan.name list * 's LinState.t lazy_t
      -> 's scatter_

type 'a scatter = 'a scatter_ Lin.lin

let scatter_ops (type b) : (module State.StateOp with type a = b scatter_) =
  let module M = struct
    type nonrec a = b scatter_

    let determinise ctx (Scatter (tag, name, cont)) =
      Scatter (tag, name, lazy (PowState.determinise_core ctx (Lazy.force cont)))

    let merge ctx (Scatter (tag1, names1, cont1))
        (Scatter (tag2, names2, cont2)) =
      assert (tag1 = tag2);
      List.iter2 DynChan.unify names1 names2;
      Scatter
        ( tag1,
          names1,
          lazy (PowState.merge_core ctx (Lazy.force cont1) (Lazy.force cont2))
        )

    let force ctx (Scatter (_, names, cont)) =
      ignore (List.map DynChan.finalise names);
      PowState.force_core ctx (Lazy.force cont)

    let to_string ctx (Scatter (_, _, cont)) =
      "."
      ^
      if Lazy.is_val cont then
        let cont = Lazy.force cont in
        PowState.to_string_core ctx cont
      else "<lazy_out_cont>"
  end in
  (module M)

let make_scatter role lab name s =
  let x =
    State.
      {
        st = Scatter (lab.method_name, name, Lazy.from_val s);
        st_ops = scatter_ops;
      }
  in
  PowState.make_deterministic (Context.new_key ())
  @@ Lazy.from_val
  @@ LinState.map_method role
  @@ LinState.map_method lab
  @@ LinState.make_lin_state x

let scatter s =
  let (Scatter (labname, names, cont)) = Lin.use s in
  let tag = Btype.hash_variant labname in
  let cont = PowState.ensure_determinised (Lazy.force cont) in
  List.iter (fun name -> DynChan.send (DynChan.finalise name) tag) names;
  Lin.fresh cont
