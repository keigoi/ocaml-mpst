module Context = State.Context

type tag = int
type 'var gather_ = (tag DynChan.name list * 'var InpChoice.t list) lazy_t
type 'var gather = 'var gather_ Lin.lin

let gather_op0 (type b) : b gather_ State.op =
  let module M = struct
    type nonrec a = b gather_

    let determinise ctx gthr =
      lazy
        begin
          let names, exts = Lazy.force gthr in
          (names, List.map (InpChoice.determinise ctx) exts)
        end

    let merge ctx g1 g2 =
      lazy
        begin
          let names1, exts1 = Lazy.force g1 and names2, exts2 = Lazy.force g2 in
          List.iter2 DynChan.unify names1 names2;
          (names1, InpChoice.merge ctx exts1 exts2)
        end

    let force ctx s =
      let (names : int DynChan.name list), extcs = Lazy.force s in
      ignore (List.map DynChan.finalise names);
      extcs |> List.iter (InpChoice.force ctx)

    let to_string ctx s =
      "?{"
      ^ (if Lazy.is_val s then
         String.concat ","
           (List.map (fun e -> InpChoice.to_string ctx e) (snd (Lazy.force s)))
        else "<lazy_inp>")
      ^ "}"
  end in
  (module M)

let gather_op role =
  LinState.gen_op @@ State.obj_op role @@ LinState.lin_op @@ gather_op0

let make_gather0 constr names s =
  Lazy.from_val (names, [ InpChoice.make constr s ])

open Rows

let make_gather role constr (names : _ DynChan.name list) s =
  let st =
    (role.make_obj |> Lin.map_gen) @@ Lin.declare @@ make_gather0 constr names s
  in
  PowState.make (gather_op role) st

let gather (inp : _ gather) =
  let names, items = Lazy.force @@ Lin.use inp in
  let tags =
    List.map (fun name -> DynChan.receive (DynChan.finalise name)) names
  in
  let tag = match tags with tag :: _ -> tag | [] -> failwith "impossible" in
  assert (List.for_all (fun t -> t = tag) tags);
  items |> List.map InpChoice.match_item |> List.assoc tag |> Lazy.force

type ('v, 's) gather_val_ = 'v DynChan.name list * 's LinState.t
type ('v, 's) gather_val = ('v, 's) gather_val_ Lin.lin

let gather_val_op0 (type v s) : (v, s) gather_val_ State.op =
  let module M = struct
    type a = (v, s) gather_val_

    let determinise ctx (ch, s) = (ch, PowState.determinise ctx s)

    let merge ctx (ch1, s1) (ch2, s2) =
      List.iter2 DynChan.unify ch1 ch2;
      (ch1, PowState.merge ctx s1 s2)

    let force ctx (_, s) = PowState.force ctx s
    let to_string ctx (_, s) = "?." ^ PowState.to_string ctx s
  end in
  (module M)

let gather_val_op role = State.obj_op role @@ LinState.lin_op gather_val_op0

let make_gather_val role names cont =
  let st = (role.make_obj |> Lin.map_gen) @@ Lin.declare (names, cont) in
  PowState.make (LinState.gen_op (gather_val_op role)) st

let gather_val (type v s) (gthrv : (v, s) gather_val) =
  let chs, cont = Lin.use gthrv in
  ( List.map (fun ch -> DynChan.receive (DynChan.finalise ch)) chs,
    Lin.fresh (PowState.ensure_determinised cont) )
