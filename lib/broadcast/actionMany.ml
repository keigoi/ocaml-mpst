type 'a many = Many of 'a State.t list

let get_many (Many ss) = List.map State.determinise ss

let det_list_ops (type b) (module D : State.DetState with type a = b) =
  let module DetList = struct
    type nonrec a = b many

    let determinise _ctx ss =
      let ss =
        List.fold_left
          (fun acc (Many s) -> List.map2 (fun x y -> y :: x) acc s)
          [] ss
      in
      let ss =
        List.map
          (fun ss -> List.fold_left State.merge (List.hd ss) (List.tl ss))
          ss
      in
      Many ss

    let force _ctx _s = ()
    let to_string _ctx _s = "<broadcast>"
  end in
  (module DetList : State.DetState with type a = b many)

let outs ~count role lab names (ss : _ many State.t) : _ many State.t =
  let ss =
    lazy
      (let (Many ss) = State.determinise ss in
       ss)
  in
  let ss =
    List.init count (fun i ->
        State.make_lazy (lazy (List.nth (Lazy.force ss) i)))
  in
  State.make_deterministic (State.Context.new_key ())
    (lazy
      State.
        {
          det_state =
            Many
              (List.mapi
                 (fun _i (name, s) -> ActionOut.out role lab name s)
                 (List.combine names ss));
          det_ops = det_list_ops (ActionOut.out_ops role lab);
        })

let inps ~count role constr names (ss : _ many State.t) : _ many State.t =
  let ss =
    lazy
      (let (Many ss) = State.determinise ss in
       ss)
  in
  let ss =
    List.init count (fun i ->
        State.make_lazy (lazy (List.nth (Lazy.force ss) i)))
  in
  State.make_deterministic (State.Context.new_key ())
    (lazy
      State.
        {
          det_state =
            Many
              (List.mapi
                 (fun _i (name, s) -> ActionInp.inp role constr name s)
                 (List.combine names ss));
          det_ops = det_list_ops (ActionInp.inp_ops role);
        })
