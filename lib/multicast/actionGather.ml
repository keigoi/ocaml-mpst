module Context = State.Context
open Rows

type tag = int
type 'var gather_ = tag DynChan.name list * 'var Extchoice.extchoice_item list
type 'var gather = 'var gather_ lazy_t

let merge role ctx s1 s2 =
  role.make_obj
  @@ lazy
       begin
         let names1, exts1 = Lazy.force @@ role.call_obj s1
         and names2, exts2 = Lazy.force @@ role.call_obj s2 in
         List.iter2 DynChan.unify names1 names2;
         (names1, Extchoice.merge_items ctx exts1 exts2)
       end

let gather_ops (type b) (role : (b, _) method_) =
  let module DetInp = struct
    type nonrec a = b

    let determinise ctx = function
      | [ s ] ->
          role.make_obj
            (lazy
              begin
                let names, exts = Lazy.force (role.call_obj s) in
                (names, List.map (Extchoice.determinise ctx) exts)
              end)
      | s :: ss -> List.fold_left (merge role ctx) s ss
      | [] -> failwith "impossible: inp_determinise"

    let force ctx s =
      let (names : int DynChan.name list), extcs = Lazy.force @@ role.call_obj s in
      ignore (List.map DynChan.finalise names);
      extcs |> List.iter (Extchoice.force ctx)

    let to_string ctx s =
      role.method_name
      ^ "?{"
      ^ (let s = role.call_obj s in
         if Lazy.is_val s then
           String.concat ","
             (List.map
                (fun e -> Extchoice.to_string ctx e)
                (snd (Lazy.force s)))
         else "<lazy_inp>")
      ^ "}"
  end in
  (module DetInp : State.DetState with type a = b)

let gather_state role constr names s =
  role.make_obj (Lazy.from_val (names, [ Extchoice.make constr s ]))

let make_gather role constr (names : _ DynChan.name list) s =
  State.make_deterministic (Context.new_key ())
  @@ Lazy.from_val
       State.
         { det_state = gather_state role constr names s; det_ops = gather_ops role }

let gather (inp : _ gather) =
  let names, items = Lazy.force inp in
  let tags =
    List.map (fun name -> DynChan.receive (DynChan.finalise name)) names
  in
  let tag = match tags with tag :: _ -> tag | [] -> failwith "impossible" in
  assert (List.for_all (fun t -> t = tag) tags);
  items |> List.map Extchoice.match_item |> List.assoc tag
