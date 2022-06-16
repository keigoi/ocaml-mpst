type 'a many = 'a LinState.t list

let get_many ss = List.map (fun s -> Lin.fresh @@ PowState.do_determinise s) ss

let list_op (type b) : b many State.op =
  let module M = struct
    type nonrec a = b many

    let determinise _ctx s = s
    let merge _ctx s1 s2 = List.map2 PowState.make_merge s1 s2
    let force _ctx _ss = ()
    let to_string _ctx _s = "<multicast>"
  end in
  (module M)

let units ~count =
  PowState.make
    (LinState.gen_op @@ list_op)
    (Lin.declare_unlimited @@ List.init count (fun _i -> LinState.unit))

let make_many ~count (names : 'v DynChan.name list) (ss : 's many LinState.t)
    (f : 'v DynChan.name -> 's LinState.t -> 'a LinState.t) : 'a many LinState.t
    =
  let ss =
    lazy
      (let ss =
         Lin.fresh @@ PowState.do_determinise ss (* FIXME this needs context *)
       in
       ss)
  in
  let ss =
    List.init count (fun i ->
        PowState.make_lazy (lazy (List.nth (Lazy.force ss) i)))
  in
  PowState.make_raw (State.Context.new_key ())
    (lazy
      State.
        {
          st = Lin.declare_unlimited @@ List.map2 f names ss;
          st_op = LinState.gen_op @@ list_op;
        })

let make_selects ~count role lab names (ss : _ many LinState.t) :
    _ many LinState.t =
  make_many ~count names ss (fun name s ->
      ActionOut.make_select role lab name s)

let make_branches ~count role constr names (ss : _ many LinState.t) =
  make_many ~count names ss (fun name s ->
      ActionInp.make_branch role constr name s)

let make_outs ~count role names ss =
  make_many ~count names ss (fun name s -> ActionOut.make_out role name s)

let make_inps ~count role names ss =
  make_many ~count names ss (fun name s -> ActionInp.make_inp role name s)
