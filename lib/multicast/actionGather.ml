module Context = State.Context
open Rows

type tag = int
type 'var gather_ = tag DynChan.name list * 'var InpChoice.t list
type 'var gather = 'var gather_ lazy_t Lin.lin

let gather_ops (type b) (role : (b, _ Lin.lin) method_) =
  let module DetGather = struct
    type nonrec a = b

    let determinise ctx s =
      let determinise_gather gthr =
        lazy
          begin
            let names, exts = Lazy.force gthr in
            (names, List.map (InpChoice.determinise ctx) exts)
          end
      in
      role.make_obj @@ Lin.map_lin determinise_gather (role.call_obj s)

    let merge ctx s1 s2 =
      let merge_gather g1 g2 =
        lazy
          begin
            let names1, exts1 = Lazy.force g1
            and names2, exts2 = Lazy.force g2 in
            List.iter2 DynChan.unify names1 names2;
            (names1, InpChoice.merge ctx exts1 exts2)
          end
      in
      role.make_obj
      @@ Lin.merge_lin merge_gather (role.call_obj s1) (role.call_obj s2)

    let force ctx s =
      let (names : int DynChan.name list), extcs =
        Lazy.force @@ Lin.raw_lin @@ role.call_obj s
      in
      ignore (List.map DynChan.finalise names);
      extcs |> List.iter (InpChoice.force ctx)

    let to_string ctx s =
      role.method_name
      ^ "?{"
      ^ (let s = role.call_obj s in
         if Lazy.is_val (Lin.raw_lin s) then
           String.concat ","
             (List.map
                (fun e -> InpChoice.to_string ctx e)
                (snd (Lazy.force (Lin.raw_lin s))))
         else "<lazy_inp>")
      ^ "}"
  end in
  LinState.det_lin_ops (module DetGather : State.DetState with type a = b)

let gather_state role constr names (s : _ LinState.t) =
  Lin.map_gen role.make_obj
  @@ Lin.declare (Lazy.from_val (names, [ InpChoice.make constr s ]))

let make_gather role constr (names : _ DynChan.name list) s =
  State.make_deterministic (Context.new_key ())
  @@ Lazy.from_val
       State.
         {
           det_state = gather_state role constr names s;
           det_ops = gather_ops role;
         }

let gather (inp : _ gather) =
  let names, items = Lazy.force @@ Lin.use inp in
  let tags =
    List.map (fun name -> DynChan.receive (DynChan.finalise name)) names
  in
  let tag = match tags with tag :: _ -> tag | [] -> failwith "impossible" in
  assert (List.for_all (fun t -> t = tag) tags);
  items |> List.map InpChoice.match_item |> List.assoc tag |> Lazy.force
