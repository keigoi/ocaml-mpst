open Rows
module Context = State.Context

type tag = int

type ('v, 's) out_ =
  | Out : string * 'v DynChan.name * 's LinState.t lazy_t -> ('v, 's) out_

type ('v, 'a) out = ('v, 'a) out_ Lin.lin
type 's select = (tag, 's) out

let out_ops_raw (type v b) () =
  let module DetOut = struct
    type nonrec a = (v, b) out_

    let determinise ctx (Out (tag, name, cont)) =
      Out (tag, name, lazy (PowState.determinise_core ctx (Lazy.force cont)))

    let merge ctx (Out (tag1, name1, cont1)) (Out (tag2, name2, cont2)) =
      assert (tag1 = tag2);
      DynChan.unify name1 name2;
      Out
        ( tag1,
          name1,
          lazy (PowState.merge_core ctx (Lazy.force cont1) (Lazy.force cont2)) )

    let force ctx (Out (_, name, cont)) =
      ignore (DynChan.finalise name);
      PowState.force_core ctx (Lazy.force cont)

    let to_string ctx (Out (_, _, cont)) =
      (*FIXME:use label*)
      if Lazy.is_val cont then
        let cont = Lazy.force cont in
        PowState.to_string_core ctx cont
      else "<lazy_out_cont>"
  end in
  (module DetOut : State.StateOp with type a = (v, b) out_)

let make_select role lab name s : _ LinState.t =
  let st =
    State.
      {
        st = Out (lab.method_name, name, Lazy.from_val s);
        st_ops = out_ops_raw ();
      }
  in
  PowState.make_deterministic (Context.new_key ())
  @@ Lazy.from_val
  @@ LinState.map role.make_obj role.call_obj (fun s -> role.method_name ^ s)
  @@ LinState.map lab.make_obj lab.call_obj (fun s -> lab.method_name ^ s)
  @@ LinState.make_lin_state st

let make_out role name s : _ LinState.t =
  let st =
    State.
      {
        st = Out (""(*FIXME*), name, Lazy.from_val s);
        st_ops = out_ops_raw ();
      }
  in
  PowState.make_deterministic (Context.new_key ())
  @@ Lazy.from_val
  @@ LinState.map role.make_obj role.call_obj (fun s -> role.method_name ^ s)
  @@ LinState.make_lin_state st

let select out =
  let (Out (labname, name, cont)) = Lin.use out in
  let tag = Btype.hash_variant labname in
  let cont = PowState.ensure_determinised (Lazy.force cont) in
  DynChan.send (DynChan.finalise name) tag;
  Lin.fresh cont

let send out v =
  let (Out (_, name, cont)) = Lin.use out in
  let cont = PowState.ensure_determinised (Lazy.force cont) in
  DynChan.send (DynChan.finalise name) v;
  Lin.fresh cont
