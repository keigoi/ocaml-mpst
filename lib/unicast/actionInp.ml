module Context = State.Context
open Rows

type tag = int
type 'var branch_ = tag DynChan.name * 'var InpChoice.t list
type 'var branch = 'var branch_ lazy_t Lin.lin

let branch_ops (type b) =
  let module M = struct
    type nonrec a = b branch_ Lazy.t

    let determinise ctx inp =
      lazy
        begin
          let name, exts = Lazy.force inp in
          (name, List.map (InpChoice.determinise ctx) exts)
        end

    let merge ctx inp1 inp2 =
      lazy
        begin
          let name1, exts1 = Lazy.force @@ inp1
          and name2, exts2 = Lazy.force @@ inp2 in
          DynChan.unify name1 name2;
          (name1, InpChoice.merge ctx exts1 exts2)
        end

    let force ctx s =
      let name, extcs = Lazy.force s in
      ignore (DynChan.finalise name);
      extcs |> List.iter (InpChoice.force ctx)

    let to_string ctx s =
      "?{"
      ^ (if Lazy.is_val s then
         String.concat ","
           (List.map (fun e -> InpChoice.to_string ctx e) (snd (Lazy.force s)))
        else "<lazy_inp>")
      ^ "}"
  end in
  (module M : State.StateOp with type a = b branch_ Lazy.t)

let branch_state constr name (s : _ LinState.t) =
  State.
    {
      st = Lazy.from_val (name, [ InpChoice.make constr s ]);
      st_ops = branch_ops;
    }

let make_branch role constr name (s : _ LinState.t) =
  State.make_deterministic (Context.new_key ())
  @@ Lazy.from_val
  @@ State.map_method role (branch_state constr name s)

let branch (inp : _ branch) =
  let name, items = Lazy.force @@ Lin.use inp in
  items
  |> List.map InpChoice.match_item
  |> List.assoc (DynChan.receive (DynChan.finalise name))
  |> Lazy.force

type ('v, 's) inp_ = 'v DynChan.name * 's LinState.t
type ('v, 's) inp = ('v, 's) inp_ Lin.lin

let inp_ops (type v s) role =
  let module DetInp = struct
    type a = (v, s) inp_

    let determinise ctx (ch, s) = (ch, State.determinise_core ctx s)

    let merge ctx (ch1, s1) (ch2, s2) =
      DynChan.unify ch1 ch2;
      (ch1, State.merge_core ctx s1 s2)

    let force ctx (_, s) = State.force_core ctx s
    let to_string ctx (_, s) = "?." ^ State.to_string_core ctx s
  end in
  LinState.det_gen_ops
  @@ State.det_wrap_obj role
  @@ LinState.det_lin_ops
       (module DetInp : State.StateOp with type a = (v, s) inp_)

let inp_state role chan cont =
  Lin.map_gen role.make_obj @@ Lin.declare (chan, cont)

let make_inp role chan cont =
  State.make_deterministic (State.Context.new_key ())
  @@ Lazy.from_val
  @@ State.{ st = inp_state role chan cont; st_ops = inp_ops role }

let receive inp =
  let ch, cont = Lin.use inp in
  ( DynChan.receive (DynChan.finalise ch),
    Lin.fresh (State.ensure_determinised cont) )
