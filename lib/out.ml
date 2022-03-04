open Rows
open State

type tag = int

module type Name = sig
  type 'a name
  type 'a endpoint

  val unify : 'a name -> 'a name -> unit
  val finalise : 'a name -> 'a endpoint
end

module Make (Name : Name) = struct
  type _ out = Out : string * tag Name.name * 's t lazy_t -> 's out

  let real_merge ctx l r =
    let idl, dl = Determinise.determinise ctx l
    and idr, dr = Determinise.determinise ctx r in
    let state_id = StateHash.make_union_keys idl idr in
    deterministic state_id (State.determinise_list_lazy ctx state_id [ dl; dr ])

  let out_merge role lab ctx s1 s2 =
    let (Out (tag1, name1, cont1)) = lab.call_obj @@ role.call_obj s1
    and (Out (tag2, name2, cont2)) = lab.call_obj @@ role.call_obj s2 in
    assert (tag1 = tag2);
    Name.unify name1 name2;
    role.make_obj
    @@ lab.make_obj
    @@ Out
         ( tag1,
           name1,
           lazy (real_merge ctx (Lazy.force cont1) (Lazy.force cont2)) )

  let out_determinise role lab ctx = function
    | [ s ] ->
        let (Out (tag, name, cont)) = lab.call_obj @@ role.call_obj s in
        role.make_obj
        @@ lab.make_obj
        @@ Out
             ( tag,
               name,
               lazy
                 (let state_id, d =
                    Determinise.determinise ctx (Lazy.force cont)
                  in
                  deterministic state_id d) )
    | t :: ts -> List.fold_left (out_merge role lab ctx) t ts
    | [] -> failwith "out_determinise: impossible"

  let out_force role lab ctx s =
    let (Out (_, name, cont)) = lab.call_obj @@ role.call_obj s in
    ignore (Name.finalise name);
    force_traverse ctx (Lazy.force cont)

  let out_to_string role lab ctx s =
    let (Out (_, _, cont)) = lab.call_obj @@ role.call_obj s in
    role.method_name
    ^ "!"
    ^ lab.method_name
    ^ "."
    ^
    if Lazy.is_val cont then
      let cont = Lazy.force cont in
      to_string ctx cont
    else "<lazy_out_cont>"
end