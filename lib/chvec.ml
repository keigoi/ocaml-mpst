open Rows
open State

module type Name = sig
  type 'a name
  type 'a endpoint

  val unify : 'a name -> 'a name -> unit
  val finalise : 'a name -> 'a endpoint
end

type tag = int

module Make (Name : Name) = struct
  type _ out = Out : string * tag Name.name * 's t lazy_t -> 's out
  and 'var inp_ = tag Name.name * 'var extchoice_item list
  and 'var inp = 'var inp_ lazy_t

  and _ extchoice_item =
    | ExternalChoiceItem : ('var, 's) constr * 's t -> 'var extchoice_item

  module OutMerge = struct
    let real_merge ctx l r =
      let idl, dl = Determinise.determinise ctx l
      and idr, dr = Determinise.determinise ctx r in
      let state_id = StateHash.union_keys idl idr in
      Deterministic (state_id, determinise_head_list ctx state_id [ dl; dr ])

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
                    Deterministic (state_id, d)) )
      | t :: ts -> List.fold_left (out_merge role lab ctx) t ts
      | [] -> failwith "out_determinise: impossible"

    let out_force role lab ctx s =
      let (Out (_, name, cont)) = lab.call_obj @@ role.call_obj s in
      ignore (Name.finalise name);
      force_determinised ctx (Lazy.force cont)

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

  module InpMerge = struct
    let try_real_merge_extchoice_item :
        type a.
        StateHash.t ->
        a extchoice_item ->
        a extchoice_item ->
        a extchoice_item option =
     fun ctx l r ->
      let ( ExternalChoiceItem (constr1, cont1),
            ExternalChoiceItem (constr2, cont2) ) =
        (l, r)
      in
      (* check if constr1 = constr2 and then merge cont1 with cont2 using
         "try_cast_and_merge_heads" above. To do so:
           (1) We need to step-determinise the continuation, as required by
               the function
           (2) Compute the new state id via 'generalised union'
               (StateId.general_union)
      *)
      (* (1) extract the channel objects ==== *)
      let state_id1, cont1 = Determinise.determinise ctx cont1 in
      let state_id2, cont2 = Determinise.determinise ctx cont2 in
      let make_ constr state_id cont =
        ExternalChoiceItem (constr, Deterministic (state_id, cont))
      in
      (* (2) compute the new state id ==== *)
      match StateHash.general_union_keys state_id1 state_id2 with
      | Left state_id ->
          try_cast_then_merge_heads ctx state_id constr1 constr2 cont1 cont2
          |> Option.map (make_ constr1 state_id)
      | Right state_id ->
          try_cast_then_merge_heads ctx state_id constr2 constr1 cont2 cont1
          |> Option.map (make_ constr2 state_id)

    let determinise_extchoice_item ctx (ExternalChoiceItem (constr, cont)) =
      let state_id, d = Determinise.determinise ctx cont in
      ExternalChoiceItem (constr, Deterministic (state_id, d))

    let rec real_inp_merge_one :
        type a.
        StateHash.t ->
        a extchoice_item list ->
        a extchoice_item ->
        a extchoice_item list =
     fun ctx inp extc_item ->
      match inp with
      | e :: inp -> (
          match try_real_merge_extchoice_item ctx e extc_item with
          | Some e -> e :: List.map (determinise_extchoice_item ctx) inp
          | None ->
              determinise_extchoice_item ctx e
              :: real_inp_merge_one ctx inp extc_item)
      | [] -> [ determinise_extchoice_item ctx extc_item ]

    let real_inp_merge ctx s1 s2 = List.fold_left (real_inp_merge_one ctx) s1 s2

    let inp_merge role ctx s1 s2 =
      role.make_obj
      @@ lazy
           begin
             let s1 = role.call_obj s1 and s2 = role.call_obj s2 in
             let name1, s1 = Lazy.force s1 and name2, s2 = Lazy.force s2 in
             Name.unify name1 name2;
             (name1, real_inp_merge ctx s1 s2)
           end

    let inp_determinise role ctx = function
      | [ s ] ->
          role.make_obj
            (lazy
              begin
                let name, exts = Lazy.force (role.call_obj s) in
                (name, List.map (determinise_extchoice_item ctx) exts)
              end)
      | s :: ss -> List.fold_left (inp_merge role ctx) s ss
      | [] -> failwith "impossible: inp_determinise"

    let inp_force role ctx s =
      let s = role.call_obj s in
      let name, extcs = Lazy.force s in
      ignore (Name.finalise name);
      extcs
      |> List.iter (fun (ExternalChoiceItem (_, s)) -> force_determinised ctx s)

    let inp_to_string role ctx s =
      role.method_name
      ^ "?{"
      ^ (let s = role.call_obj s in
         if Lazy.is_val s then
           String.concat ","
             (List.map
                (fun (ExternalChoiceItem (constr, cont)) ->
                  constr.constr_name ^ "." ^ to_string ctx cont)
                (snd @@ Lazy.force s))
         else "<lazy_inp>")
      ^ "}"
  end

  let out role lab name s =
    Deterministic
      ( StateHash.make_key (),
        Lazy.from_val
          {
            head =
              role.make_obj
              @@ lab.make_obj
              @@ Out (lab.method_name, name, Lazy.from_val s);
            determinise_list = OutMerge.out_determinise role lab;
            force_determinised = OutMerge.out_force role lab;
            to_string = OutMerge.out_to_string role lab;
          } )

  let inp role constr name s =
    Deterministic
      ( StateHash.make_key (),
        Lazy.from_val
          {
            head =
              role.make_obj @@ lazy (name, [ ExternalChoiceItem (constr, s) ]);
            determinise_list = InpMerge.inp_determinise role;
            force_determinised = InpMerge.inp_force role;
            to_string = InpMerge.inp_to_string role;
          } )
end

module Sync = struct
  include Name
  module M = Make (Name)
  include M

  type chan = unit

  let make () = ()

  let select (Out (labname, name, cont)) =
    let tag = Btype.hash_variant labname in
    match Lazy.force cont with
    | Deterministic (_, cont) ->
        assert (Lazy.is_val cont);
        Event.sync (Event.send (Name.finalise name) tag);
        (Lazy.force cont).head
    | _ -> failwith "select: not determinised. possible determinisation bug?"

  let branch (inp : _ inp) =
    let make_event (ExternalChoiceItem (var, cont)) =
      match cont with
      | Deterministic (_, cont) ->
          ( Btype.hash_variant var.constr_name,
            var.make_var (Lazy.force cont).head )
      | _ -> failwith "branch: not determinised. possible determinisaton bug?"
    in
    let name, items = Lazy.force inp in
    items
    |> List.map make_event
    |> List.assoc (Event.sync (Event.receive (Name.finalise name)))
end

module Async = struct
  include DynChan
  module M = Make (DynChan)
  include M

  let select (Out (labname, name, cont)) =
    let tag = Btype.hash_variant labname in
    match Lazy.force cont with
    | Deterministic (_, cont) ->
        assert (Lazy.is_val cont);
        DynChan.send (DynChan.finalise name) tag;
        (Lazy.force cont).head
    | _ -> failwith "select: not determinised. possible determinisation bug?"

  let branch (inp : _ inp) =
    let make_event (ExternalChoiceItem (var, cont)) =
      match cont with
      | Deterministic (_, cont) ->
          ( Btype.hash_variant var.constr_name,
            var.make_var (Lazy.force cont).head )
      | _ -> failwith "branch: not determinised. possible determinisaton bug?"
    in
    let name, items = Lazy.force inp in
    items
    |> List.map make_event
    |> List.assoc (DynChan.receive (DynChan.finalise name))
end
