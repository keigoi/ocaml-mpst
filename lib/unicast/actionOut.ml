open Rows
module Context = State.Context

type tag = int
type ('v, 's) out_ = string * 'v DynChan.endpoint * 's LinState.t lazy_t
type ('v, 'a) out = ('v, 'a) out_ Lin.lin
type 's select = (tag, 's) out

let out_op0 (type v cont) : (v, cont) out_ State.op =
  let module M = struct
    type nonrec a = (v, cont) out_

    let determinise ctx (tag, name, (lazy cont)) =
      (tag, name, lazy (PowState.determinise ctx cont))

    let flatten ctx (tag, name, (lazy cont)) =
      (tag, name, lazy (PowState.flatten ctx cont))

    let merge ctx (tag1, name1, cont1) (tag2, name2, cont2) =
      assert (tag1 = tag2);
      DynChan.unify name1 name2;
      ( tag1,
        name1,
        lazy (PowState.merge ctx (Lazy.force cont1) (Lazy.force cont2)) )

    let force ctx (_, name, cont) =
      ignore (DynChan.finalise name);
      PowState.force ctx (Lazy.force cont)

    let to_string ctx (_, _, cont) =
      (*FIXME:use label*)
      if Lazy.is_val cont then
        let cont = Lazy.force cont in
        PowState.to_string ctx cont
      else "<lazy_out_cont>"
  end in
  (module M)

let out_op role = State.obj_op role @@ LinState.lin_op out_op0

let select_op role label =
  State.obj_op role @@ State.obj_op label @@ LinState.lin_op out_op0

let make_out role name s : _ LinState.t =
  let st =
    (role.make_obj |> Lin.map_gen)
    @@ Lin.declare ("_out" (* FIXME *), name, Lazy.from_val s)
  in
  let op = LinState.gen_op (out_op role) in
  PowState.make op st

let make_select role label name s : _ LinState.t =
  let st =
    Lin.map_gen (fun t -> role.make_obj @@ label.make_obj t)
    @@ Lin.declare (label.method_name, name, Lazy.from_val s)
  in
  let op = LinState.gen_op @@ select_op role label in
  PowState.make op st

let select out =
  let labname, name, cont = Lin.use out in
  let tag = Btype.hash_variant labname in
  let cont = PowState.ensure_determinised (Lazy.force cont) in
  DynChan.send (DynChan.finalise name) tag;
  Lin.fresh cont

let send out v =
  let _, name, cont = Lin.use out in
  let cont = PowState.ensure_determinised (Lazy.force cont) in
  DynChan.send (DynChan.finalise name) v;
  Lin.fresh cont
