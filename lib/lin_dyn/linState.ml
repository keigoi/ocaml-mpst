type 'a t = 'a Lin.gen State.t

let det_lin_ops (type b) (module D : State.DetState with type a = b) =
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
  (module DetList : State.DetState with type a = b Lin.gen)

let unit =
  State.make_deterministic (State.Context.new_key ())
  @@ Lazy.from_val
       State.
         {
           det_state = Lin.declare_unlimited ();
           det_ops = det_lin_ops (module State.Unit);
         }
