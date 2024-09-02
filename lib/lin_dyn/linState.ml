type 'a t = 'a Lin.gen PowState.t

let lin_op (type b) (module D : State.Op with type a = b) : b Lin.lin State.op =
  let module M = struct
    type nonrec a = b Lin.lin

    let determinise ctx s = Lin.map_lin (D.determinise ctx) s

    let flatten ctx s = Lin.map_lin (D.flatten ctx) s

    let merge ctx (s1 : b Lin.lin) (s2 : b Lin.lin) =
      Lin.merge_lin (D.merge ctx) s1 s2

    let force ctx s =
      let s = Lin.raw_lin s in
      D.force ctx s

    let to_string ctx s = D.to_string ctx (Lin.raw_lin s)
  end in
  (module M)

let gen_op (type b) (module D : State.Op with type a = b) : b Lin.gen State.op =
  let module M = struct
    type nonrec a = b Lin.gen

    let determinise ctx s = Lin.map_gen (D.determinise ctx) s

    let flatten ctx s = Lin.map_gen (D.flatten ctx) s

    let merge ctx (s1 : b Lin.gen) (s2 : b Lin.gen) =
      Lin.merge_gen (D.merge ctx) s1 s2

    let force ctx s =
      let s = Lin.raw_gen s in
      D.force ctx s

    let to_string ctx s = D.to_string ctx (Lin.raw_gen s)
  end in
  (module M)

let unit =
  PowState.make (gen_op (module State.Unit)) @@ Lin.declare_unlimited ()
