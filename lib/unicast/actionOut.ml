open Rows
module Context = State.Context

type tag = int

type _ out_ =
  | Out : string * tag DynChan.name * 's LinState.t lazy_t -> 's out_

type 'a out = 'a out_ Lin.lin

let merge_out ctx (Out (tag1, name1, cont1)) (Out (tag2, name2, cont2)) =
  let merge_cont ctx l r =
    let idl, dl = State.determinise_core_ ctx l
    and idr, dr = State.determinise_core_ ctx r in
    let state_id = Context.union_keys idl idr in
    State.make_deterministic state_id (State.merge_det ctx state_id dl dr)
  in
  assert (tag1 = tag2);
  DynChan.unify name1 name2;
  Out (tag1, name1, lazy (merge_cont ctx (Lazy.force cont1) (Lazy.force cont2)))

let determinise_out ctx (Out (tag, name, cont)) =
  Out (tag, name, lazy (State.determinise_core ctx (Lazy.force cont)))

let out_ops (type b) (role : (b, _) method_) lab =
  let module DetOut = struct
    type nonrec a = b

    let determinise ctx s =
      let out = lab.call_obj @@ role.call_obj s in
      role.make_obj @@ lab.make_obj @@ Lin.map_lin (determinise_out ctx) out

    let merge ctx s1 s2 =
      let out1 = lab.call_obj (role.call_obj s1)
      and out2 = lab.call_obj (role.call_obj s2) in
      role.make_obj @@ lab.make_obj @@ Lin.merge_lin (merge_out ctx) out1 out2

    let force ctx s =
      let (Out (_, name, cont)) =
        Lin.raw_lin @@ lab.call_obj (role.call_obj s)
      in
      ignore (DynChan.finalise name);
      State.force_core ctx (Lazy.force cont)

    let to_string ctx s =
      let (Out (_, _, cont)) = Lin.raw_lin @@ lab.call_obj (role.call_obj s) in
      role.method_name
      ^ "!"
      ^ lab.method_name
      ^ "."
      ^
      if Lazy.is_val cont then
        let cont = Lazy.force cont in
        State.to_string_core ctx cont
      else "<lazy_out_cont>"
  end in
  LinState.det_lin_ops (module DetOut : State.DetState with type a = b)

let out_state role lab name s =
  Lin.declare (Out (lab.method_name, name, Lazy.from_val s))
  |> Lin.map_gen (fun out -> role.make_obj (lab.make_obj out))

let make_out role lab name s =
  State.make_deterministic (Context.new_key ())
  @@ Lazy.from_val
       State.
         { det_state = out_state role lab name s; det_ops = out_ops role lab }

let select out =
  let (Out (labname, name, cont)) = Lin.use out in
  let tag = Btype.hash_variant labname in
  let cont = State.ensure_determinised (Lazy.force cont) in
  DynChan.send (DynChan.finalise name) tag;
  Lin.fresh cont
