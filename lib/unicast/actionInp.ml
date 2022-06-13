module Context = State.Context
open Rows

type tag = int
type 'var branch_ = (tag DynChan.name * 'var InpChoice.t list) Lazy.t
type 'var branch = 'var branch_ Lin.lin

let branch_ops (type b) : (module State.StateOp with type a = b branch_) =
  let module M = struct
    type nonrec a = b branch_

    let merge ctx inp1 inp2 =
      lazy
        begin
          let (lazy (name1, exts1)), (lazy (name2, exts2)) = (inp1, inp2) in
          DynChan.unify name1 name2;
          (name1, InpChoice.merge ctx exts1 exts2)
        end

    let determinise ctx inp =
      lazy
        begin
          let (lazy (name, exts)) = inp in
          (name, List.map (InpChoice.determinise ctx) exts)
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

let make_branch role constr name (s : _ LinState.t) =
  let st =
    State.
      {
        st = Lazy.from_val (name, [ InpChoice.make constr s ]);
        st_ops = branch_ops;
      }
  in
  PowState.make_deterministic (Context.new_key ())
  @@ Lazy.from_val
  @@ LinState.map role.make_obj role.call_obj (fun s -> role.method_name ^ s)
  @@ LinState.make_lin_state st

let branch (inp : _ branch) =
  let name, items = Lazy.force @@ Lin.use inp in
  items
  |> List.map InpChoice.match_item
  |> List.assoc (DynChan.receive (DynChan.finalise name))
  |> Lazy.force

type ('v, 's) inp_ = 'v DynChan.name * 's LinState.t
type ('v, 's) inp = ('v, 's) inp_ Lin.lin

let inp_ops (type v s) =
  let module DetInp = struct
    type a = (v, s) inp_

    let determinise ctx (ch, s) = (ch, PowState.determinise ctx s)

    let merge ctx (ch1, s1) (ch2, s2) =
      DynChan.unify ch1 ch2;
      (ch1, PowState.merge ctx s1 s2)

    let force ctx (_, s) = PowState.force ctx s
    let to_string ctx (_, s) = "?." ^ PowState.to_string ctx s
  end in
  (module DetInp : State.StateOp with type a = (v, s) inp_)

let make_inp role chan cont =
  let st = State.{ st = (chan, cont); st_ops = inp_ops } in
  PowState.make_deterministic (State.Context.new_key ())
  @@ Lazy.from_val
  @@ LinState.map role.make_obj role.call_obj (fun s -> role.method_name ^ s)
  @@ LinState.make_lin_state st

let receive inp =
  let ch, cont = Lin.use inp in
  ( DynChan.receive (DynChan.finalise ch),
    Lin.fresh (PowState.ensure_determinised cont) )
