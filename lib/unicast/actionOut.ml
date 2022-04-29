open Rows
module Context = State.Context

type tag = int

type _ select_ =
  | Out : string * tag DynChan.name * 's LinState.t lazy_t -> 's select_

type 'a select = 'a select_ Lin.lin

let determinise_select ctx (Out (tag, name, cont)) =
  Out (tag, name, lazy (State.determinise_core ctx (Lazy.force cont)))

let select_ops (type b) role label =
  let module DetOut = struct
    type nonrec a = b select_

    let determinise ctx s = determinise_select ctx s

    let merge ctx (Out (tag1, name1, cont1)) (Out (tag2, name2, cont2)) =
      assert (tag1 = tag2);
      DynChan.unify name1 name2;
      Out
        ( tag1,
          name1,
          lazy (State.merge_core ctx (Lazy.force cont1) (Lazy.force cont2)) )

    let force ctx (Out (_, name, cont)) =
      ignore (DynChan.finalise name);
      State.force_core ctx (Lazy.force cont)

    let to_string ctx (Out (_, _, cont)) =
      if Lazy.is_val cont then
        let cont = Lazy.force cont in
        State.to_string_core ctx cont
      else "<lazy_out_cont>"
  end in
  LinState.det_gen_ops
  @@ State.det_wrap_obj role
  @@ State.det_wrap_obj label
  @@ LinState.det_lin_ops
       (module DetOut : State.DetState with type a = b select_)

let select_state role lab name s =
  Lin.declare (Out (lab.method_name, name, Lazy.from_val s))
  |> Lin.map_gen (fun out -> role.make_obj (lab.make_obj out))

let make_select role lab name s =
  State.make_deterministic (Context.new_key ())
  @@ Lazy.from_val
       State.
         {
           det_state = select_state role lab name s;
           det_ops = select_ops role lab;
         }

let select out =
  let (Out (labname, name, cont)) = Lin.use out in
  let tag = Btype.hash_variant labname in
  let cont = State.ensure_determinised (Lazy.force cont) in
  DynChan.send (DynChan.finalise name) tag;
  Lin.fresh cont
