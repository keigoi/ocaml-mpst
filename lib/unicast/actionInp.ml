module Context = State.Context
open Rows

type tag = int
type 'var branch_ = (tag DynChan.endpoint * 'var InpChoice.t list) Lazy.t
type 'var branch = 'var branch_ Lin.lin

let make_branch0 constr name s =
  Lazy.from_val (name, [ InpChoice.make constr s ])

let branch_op0 (type vars) : vars branch_ State.op =
  let module M = struct
    type nonrec a = vars branch_

    let determinise ctx inp =
      lazy
        begin
          let (lazy (name, exts)) = inp in
          (name, List.map (InpChoice.determinise ctx) exts)
        end

    let flatten ctx inp =
      lazy 
        begin
          let (lazy (name, exts)) = inp in
          (name, List.map (InpChoice.flatten ctx) exts)
        end

    let merge ctx inp1 inp2 =
      lazy
        begin
          let (lazy (name1, exts1)), (lazy (name2, exts2)) = (inp1, inp2) in
          DynChan.unify name1 name2;
          (name1, InpChoice.merge ctx exts1 exts2)
        end

    let force ctx inp =
      let (lazy (name, exts)) = inp in
      ignore (DynChan.finalise name);
      exts |> List.iter (InpChoice.force ctx)

    let to_string ctx s =
      "?{"
      ^ (if Lazy.is_val s then
         String.concat ","
           (List.map (InpChoice.to_string ctx) (snd (Lazy.force s)))
        else "<lazy_inp>")
      ^ "}"
  end in
  (module M)

let branch_op role =
  LinState.gen_op @@ State.obj_op role @@ LinState.lin_op branch_op0

let make_branch role constr name (s : _ LinState.t) =
  let st =
    (role.make_obj |> Lin.map_gen) @@ Lin.declare (make_branch0 constr name s)
  in
  PowState.make (branch_op role) st

let branch (inp : _ branch) =
  let name, items = Lazy.force @@ Lin.use inp in
  items
  |> List.map InpChoice.match_item
  |> List.assoc (DynChan.receive (DynChan.finalise name))
  |> Lazy.force

type ('v, 's) inp_ = 'v DynChan.endpoint * 's LinState.t
type ('v, 's) inp = ('v, 's) inp_ Lin.lin

let inp_op0 (type v s) : (v, s) inp_ State.op =
  let module M = struct
    type a = (v, s) inp_

    let determinise ctx (ch, s) = (ch, PowState.determinise ctx s)

    let flatten ctx (ch, s) = (ch, PowState.flatten ctx s)

    let merge ctx (ch1, s1) (ch2, s2) =
      DynChan.unify ch1 ch2;
      (ch1, PowState.merge ctx s1 s2)

    let force ctx (_, s) = PowState.force ctx s
    let to_string ctx (_, s) = "?." ^ PowState.to_string ctx s
  end in
  (module M)

let inp_op role = State.obj_op role @@ LinState.lin_op inp_op0

let make_inp role chan cont =
  let st = (role.make_obj |> Lin.map_gen) @@ Lin.declare (chan, cont) in
  PowState.make (LinState.gen_op (inp_op role)) st

let receive inp =
  let ch, cont = Lin.use inp in
  ( DynChan.receive (DynChan.finalise ch),
    Lin.fresh (PowState.ensure_determinised cont) )
