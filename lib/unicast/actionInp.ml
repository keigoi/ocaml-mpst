module Context = State.Context
open Rows

type tag = int
type 'var inp_ = tag DynChan.name * 'var InpChoice.t list
type 'var inp = 'var inp_ lazy_t Lin.lin

let inp_ops (type b) (role : (b, _) method_) =
  let module DetInp = struct
    type nonrec a = b

    let determinise ctx s =
      let determinise_inp ctx inp =
        lazy
          begin
            let name, exts = Lazy.force inp in
            (name, List.map (InpChoice.determinise ctx) exts)
          end
      in
      let inp = role.call_obj s in
      role.make_obj @@ Lin.map_lin (determinise_inp ctx) inp

    let merge ctx s1 s2 =
      let merge_inp inp1 inp2 =
        lazy
          begin
            let name1, exts1 = Lazy.force @@ inp1
            and name2, exts2 = Lazy.force @@ inp2 in
            DynChan.unify name1 name2;
            (name1, InpChoice.merge ctx exts1 exts2)
          end
      in
      let inp1 = role.call_obj s1 and inp2 = role.call_obj s2 in
      role.make_obj @@ Lin.merge_lin merge_inp inp1 inp2

    let force ctx s =
      let name, extcs = Lazy.force @@ Lin.raw_lin @@ role.call_obj s in
      ignore (DynChan.finalise name);
      extcs |> List.iter (InpChoice.force ctx)

    let to_string ctx s =
      role.method_name
      ^ "?{"
      ^ (let s = Lin.raw_lin @@ role.call_obj s in
         if Lazy.is_val s then
           String.concat ","
             (List.map
                (fun e -> InpChoice.to_string ctx e)
                (snd (Lazy.force s)))
         else "<lazy_inp>")
      ^ "}"
  end in
  LinState.det_lin_ops (module DetInp : State.DetState with type a = b)

let inp_state role constr name (s : _ LinState.t) =
  Lin.map_gen role.make_obj
  @@ Lin.declare (Lazy.from_val (name, [ InpChoice.make constr s ]))

let make_inp role constr name (s : _ LinState.t) =
  State.make_deterministic (Context.new_key ())
  @@ Lazy.from_val
       State.
         { det_state = inp_state role constr name s; det_ops = inp_ops role }

let branch (inp : _ inp) =
  let name, items = Lazy.force @@ Lin.use inp in
  items
  |> List.map InpChoice.match_item
  |> List.assoc (DynChan.receive (DynChan.finalise name))
  |> Lazy.force
