type 'a t = 'a Lin.gen State.t

let lin_ops (type b) (module D : State.StateOp with type a = b) =
  let module M = struct
    type nonrec a = b Lin.lin

    let determinise ctx s = Lin.map_lin (D.determinise ctx) s

    let merge ctx (s1 : b Lin.lin) (s2 : b Lin.lin) =
      Lin.merge_lin (D.merge ctx) s1 s2

    let force ctx s =
      let s = Lin.raw_lin s in
      D.force ctx s

    let to_string ctx s = D.to_string ctx (Lin.raw_lin s)
  end in
  (module M : State.StateOp with type a = b Lin.lin)

let gen_ops (type b) (module D : State.StateOp with type a = b) =
  let module DetList = struct
    type nonrec a = b Lin.gen

    let determinise ctx s = Lin.map_gen (D.determinise ctx) s

    let merge ctx (s1 : b Lin.gen) (s2 : b Lin.gen) =
      Lin.merge_gen (D.merge ctx) s1 s2

    let force ctx s =
      let s = Lin.raw_gen s in
      D.force ctx s

    let to_string ctx s = D.to_string ctx (Lin.raw_gen s)
  end in
  (module DetList : State.StateOp with type a = b Lin.gen)

let make_lin_state (type a) (x : a State.t) : a Lin.lin Lin.gen State.t
    =
  State.{ st = Lin.declare x.st; st_ops = gen_ops @@ lin_ops @@ x.st_ops }

let map (type a b) (f : a -> b) (g : b -> a) (name : string -> string)
    (x : a Lin.gen State.t) : b Lin.gen State.t =
  State.
    {
      st = Lin.map_gen f @@ x.st;
      st_ops = State.map_ops (Lin.map_gen f) (Lin.map_gen g) name x.st_ops;
    }

let unit =
  PowState.make_deterministic (State.Context.new_key ())
  @@ Lazy.from_val
       State.
         { st = Lin.declare_unlimited (); st_ops = gen_ops (module State.Unit) }
