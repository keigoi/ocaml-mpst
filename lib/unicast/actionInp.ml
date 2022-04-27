module Context = State.Context
open Rows

type tag = int
type 'var inp_ = tag DynChan.name * 'var Extchoice.t list
type 'var inp = 'var inp_ lazy_t

let inp_ops (type a) (role : (a, _) method_) =
  let module DetInp = struct
    type nonrec a = a

    let determinise ctx s =
      role.make_obj
        (lazy
          begin
            let name, exts = Lazy.force (role.call_obj s) in
            (name, List.map (Extchoice.determinise ctx) exts)
          end)

    let merge ctx s1 s2 =
      role.make_obj
      @@ lazy
           begin
             let name1, exts1 = Lazy.force @@ role.call_obj s1
             and name2, exts2 = Lazy.force @@ role.call_obj s2 in
             DynChan.unify name1 name2;
             (name1, Extchoice.merge ctx exts1 exts2)
           end

    let force ctx s =
      let name, extcs = Lazy.force @@ role.call_obj s in
      ignore (DynChan.finalise name);
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
  (module DetInp : State.DetState with type a = a)

let inp_state role constr name s =
  role.make_obj (Lazy.from_val (name, [ Extchoice.make constr s ]))

let make_inp role constr name s =
  State.make_deterministic (Context.new_key ())
  @@ Lazy.from_val
       State.
         { det_state = inp_state role constr name s; det_ops = inp_ops role }

let branch (inp : _ inp) =
  let name, items = Lazy.force inp in
  items
  |> List.map Extchoice.match_item
  |> List.assoc (DynChan.receive (DynChan.finalise name))
