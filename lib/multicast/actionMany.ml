type 'a many = 'a LinState.t list

let get_many ss = List.map (fun s -> Lin.fresh @@ PowState.do_determinise s) ss

let det_list_ops (type b) : b many State.op =
  let module M = struct
    type nonrec a = b many

    let determinise _ctx s = s
    let merge _ctx s1 s2 = List.map2 PowState.make_merge s1 s2
    let force _ctx _ss = ()
    (* let ss = List.map (State.determinise_core (State.Context.make ())) ss in
       List.iter (State.force_core (State.Context.make ())) ss *)

    let to_string _ctx _s = "<multicast>"
  end in
  (module M)

let make_list ~count ~f =
  lazy
    State.
      {
        st = Lin.declare_unlimited @@ List.init count f;
        st_ops = LinState.gen_op @@ det_list_ops;
      }

let units ~count =
  PowState.make_deterministic (State.Context.new_key ())
  @@ make_list ~count ~f:(fun _i -> LinState.unit)

let outs ~count role lab names (ss : _ many LinState.t) : _ many LinState.t =
  let ss =
    lazy
      (let ss = Lin.fresh @@ PowState.do_determinise ss in
       ss)
  in
  let ss =
    List.init count (fun i ->
        PowState.make_lazy (lazy (List.nth (Lazy.force ss) i)))
  in
  PowState.make_deterministic (State.Context.new_key ())
    (lazy
      State.
        {
          st =
            Lin.declare_unlimited
            @@ List.mapi
                 (fun _i (name, s) -> ActionOut.make_select role lab name s)
                 (List.combine names ss);
          st_ops = LinState.gen_op @@ det_list_ops;
        })

let inps ~count role constr names (ss : _ many LinState.t) : _ many LinState.t =
  let ss =
    lazy
      (let ss = Lin.fresh @@ PowState.do_determinise ss in
       ss)
  in
  let ss =
    List.init count (fun i ->
        PowState.make_lazy (lazy (List.nth (Lazy.force ss) i)))
  in
  PowState.make_deterministic (State.Context.new_key ())
    (lazy
      State.
        {
          st =
            Lin.declare_unlimited
            @@ List.mapi
                 (fun _i (name, s) -> ActionInp.make_branch role constr name s)
                 (List.combine names ss);
          st_ops = LinState.gen_op @@ det_list_ops;
        })
