open Types

type 'a head = 'a StateHash.head = {
  head : 'a;
  determinise_list : StateHash.t -> 'a list -> 'a;
  force_determinised : StateHash.t -> 'a -> unit;
  to_string : StateHash.t -> 'a -> string;
}

type 't state_id = 't StateHash.state_id

type _ t =
  | Deterministic : 'obj state_id * 'obj head lazy_t -> 'obj t
      (** A state with deterministic transitions. Note that the following states
          are not necessarily deterministic. *)
  | Epsilon : 'a t list -> 'a t  (** Epsilon transitions (i.e. merging) *)
  | InternalChoice : 'lr state_id * ('lr, 'l, 'r) disj * 'l t * 'r t -> 'lr t
      (** Internal choice, splitted by a disjoint concatenation (disj) *)
  | Loop : 'a t lazy_t -> 'a t  (** Start of the fix-loop *)

and _ out = Out : unit Name.t * 's t lazy_t -> 's out
and 'var inp_ = 'var extchoice_item list
and 'var inp = 'var inp_ lazy_t

and _ extchoice_item =
  | ExternalChoiceItem :
      ('var, 's) constr * unit Name.t * 's t
      -> 'var extchoice_item

exception UnguardedLoop of string

let select (Out (name, cont)) =
  match Lazy.force cont with
  | Deterministic (_, cont) ->
      assert (Lazy.is_val cont);
      Event.sync (Event.send (Name.finalise name) ());
      (Lazy.force cont).head
  | _ -> failwith "select: not determinised. possible determinisation bug?"

let branch (inp : _ inp) =
  let make_event (ExternalChoiceItem (var, n, cont)) =
    match cont with
    | Deterministic (_, cont) ->
        assert (Lazy.is_val cont);
        Event.wrap
          (Event.receive (Name.finalise n))
          (fun () -> var.make_var (Lazy.force cont).head)
    | _ -> failwith "branch: not determinised. possible determinisaton bug?"
  in
  Event.sync (Event.choose (List.map make_event (Lazy.force inp)))

let internal_choice disj l r = InternalChoice (StateHash.make_key (), disj, l, r)
let merge l r = Epsilon [ l; r ]
let loop t = Loop t

let determinise_head_list ctx id hds =
  match StateHash.lookup ctx id with
  | Some v -> v
  | None ->
      let hd =
        lazy
          (let hd = Lazy.force (List.hd hds) in
           let body =
             hd.determinise_list ctx
               (List.map (fun hd -> (Lazy.force hd).head) hds)
           in
           {
             head = body;
             determinise_list = hd.determinise_list;
             force_determinised = hd.force_determinised;
             to_string = hd.to_string;
           })
      in
      StateHash.add_binding ctx id hd;
      hd

let try_cast_then_merge_heads ctx id constrA constrB headA headB =
  match StateHash.lookup ctx id with
  | Some v -> Some v
  | None -> (
      let headA = Lazy.force headA and headB = Lazy.force headB in
      match Types.cast_if_constrs_are_same constrA constrB headB.head with
      | Some contB_body ->
          let head = headA.determinise_list ctx [ headA.head; contB_body ] in
          let head = Lazy.from_val { headA with head } in
          StateHash.add_binding ctx id head;
          Some head
      | None -> None)

let force_determinised ctx t =
  match t with
  | Deterministic (sid, h) when StateHash.lookup ctx sid = None ->
      StateHash.add_binding ctx sid h;
      let h = Lazy.force h in
      h.force_determinised ctx h.head
  | Deterministic (_, _) -> ()
  | _ -> failwith "Impossible: force: channel not determinised"

let rec to_string : 'a. StateHash.t -> 'a t -> string =
 fun ctx -> function
  | Deterministic (sid, hd) ->
      if Lazy.is_val hd then (
        StateHash.add_binding ctx sid hd;
        let hd = Lazy.force hd in
        hd.to_string ctx hd.head)
      else "<lazy_det>"
  | Epsilon ts -> List.map (to_string ctx) ts |> String.concat " [#] "
  | InternalChoice (_, _, l, r) ->
      let lstr = to_string ctx l and rstr = to_string ctx r in
      lstr ^ " (+#) " ^ rstr
  | Loop t ->
      if Lazy.is_val t then to_string ctx (Lazy.force t) else "<lazy_loop>"

module Determinise : sig
  val determinise : StateHash.t -> 's t -> 's state_id * 's head lazy_t
end = struct
  type 'a merged_or_backward_epsilon =
    ('a state_id * 'a head lazy_t list, 'a t list) Either.t

  let rec mem_phys k = function
    | x :: xs -> k == x || mem_phys k xs
    | [] -> false

  let fail_unguarded str = raise (UnguardedLoop str)

  let rec determinise : 's. StateHash.t -> 's t -> 's state_id * 's head lazy_t
      =
   fun ctx st ->
    match epsilon_closure ~ctx ~visited:[] st with
    | Left (sid, hds) -> (sid, determinise_head_list ctx sid hds)
    | Right _ -> fail_unguarded "determinise: unguarded"

  and epsilon_closure :
      type a.
      ctx:StateHash.t -> visited:a t list -> a t -> a merged_or_backward_epsilon
      =
    let ret_merged x = Either.Left x
    and ret_backward_epsilon x = Either.Right x in
    let detect_cycles ~visited backward =
      let backward =
        (* filter out backward epsilon transitions pointing to known states *)
        List.filter (fun id -> mem_phys id visited) backward
      in
      if List.length backward > 0 then
        (* there're backward epsilons yet -- return it *)
        ret_backward_epsilon backward
      else
        (* no backward epsilons anymore: unguarded recursion! *)
        fail_unguarded "epsilon_closure: unguarded"
    in
    fun ~ctx ~visited st ->
      if mem_phys st visited then ret_backward_epsilon [ st ]
      else
        match st with
        | Deterministic (sid, h) -> ret_merged (sid, [ h ])
        | Loop t -> (
            (* beginning of the fix-loop. *)
            match
              epsilon_closure ~ctx ~visited:(st :: visited) (Lazy.force t)
            with
            | Left (sid, hd) -> Left (sid, hd)
            | Right backward -> detect_cycles ~visited backward)
        | Epsilon sts ->
            (* epsilon transitions: compute epsilon-closure *)
            let hds, backward =
              List.partition_map
                (epsilon_closure ~ctx ~visited:(st :: visited))
                sts
            in
            if List.length hds > 0 then
              (* concrete transitons found - return the merged state ==== *)
              let ids, hds = List.split hds in
              let id =
                List.fold_left StateHash.union_keys (List.hd ids) (List.tl ids)
              in
              ret_merged (id, List.concat hds)
            else
              (* all transitions are epsilon - verify guardedness ==== *)
              let backward = List.concat backward in
              detect_cycles ~visited backward
        | InternalChoice (sid, disj, tl, tr) ->
            let head =
              lazy
                (let _idl, hl = determinise ctx tl
                 and _idr, hr = determinise ctx tr in
                 let hl = Lazy.force hl and hr = Lazy.force hr in
                 let bin_merge ctx lr1 lr2 =
                   (* merge splitted ones *)
                   let l =
                     hl.determinise_list ctx
                       [ disj.disj_splitL lr1; disj.disj_splitL lr2 ]
                   in
                   let r =
                     hr.determinise_list ctx
                       [ disj.disj_splitR lr1; disj.disj_splitR lr2 ]
                   in
                   (* then type-concatenate it *)
                   disj.disj_concat l r
                 in
                 let determinise_list ctx lrs =
                   List.fold_left (bin_merge ctx) (List.hd lrs) (List.tl lrs)
                 in
                 let force_determinised ctx lr =
                   hl.force_determinised ctx (disj.disj_splitL lr);
                   hr.force_determinised ctx (disj.disj_splitR lr)
                 in
                 let to_string ctx lr =
                   hl.to_string ctx (disj.disj_splitL lr)
                   ^ " (+) "
                   ^ hr.to_string ctx (disj.disj_splitR lr)
                 in
                 let tlr = disj.disj_concat hl.head hr.head in
                 { head = tlr; determinise_list; force_determinised; to_string })
            in
            ret_merged (sid, [ head ])
end

module OutMerge = struct
  let real_merge ctx l r =
    let idl, dl = Determinise.determinise ctx l
    and idr, dr = Determinise.determinise ctx r in
    let state_id = StateHash.union_keys idl idr in
    Deterministic (state_id, determinise_head_list ctx state_id [ dl; dr ])

  let out_merge role lab ctx s1 s2 =
    let (Out (name1, cont1)) = lab.call_obj @@ role.call_obj s1
    and (Out (name2, cont2)) = lab.call_obj @@ role.call_obj s2 in
    Name.unify name1 name2;
    role.make_obj
    @@ lab.make_obj
    @@ Out (name1, lazy (real_merge ctx (Lazy.force cont1) (Lazy.force cont2)))

  let out_determinise role lab ctx = function
    | [ s ] ->
        let (Out (name, cont)) = lab.call_obj @@ role.call_obj s in
        role.make_obj
        @@ lab.make_obj
        @@ Out
             ( name,
               lazy
                 (let state_id, d =
                    Determinise.determinise ctx (Lazy.force cont)
                  in
                  Deterministic (state_id, d)) )
    | t :: ts -> List.fold_left (out_merge role lab ctx) t ts
    | [] -> failwith "out_determinise: impossible"

  let out_force role lab ctx s =
    let (Out (name, cont)) = lab.call_obj @@ role.call_obj s in
    ignore (Name.finalise name);
    force_determinised ctx (Lazy.force cont)

  let out_to_string role lab ctx s =
    let (Out (_, cont)) = lab.call_obj @@ role.call_obj s in
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
    let ( ExternalChoiceItem (constr1, name1, cont1),
          ExternalChoiceItem (constr2, name2, cont2) ) =
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
      Name.unify name1 name2;
      ExternalChoiceItem (constr, name1, Deterministic (state_id, cont))
    in
    (* (2) compute the new state id ==== *)
    match StateHash.general_union_keys state_id1 state_id2 with
    | Left state_id ->
        try_cast_then_merge_heads ctx state_id constr1 constr2 cont1 cont2
        |> Option.map (make_ constr1 state_id)
    | Right state_id ->
        try_cast_then_merge_heads ctx state_id constr2 constr1 cont2 cont1
        |> Option.map (make_ constr2 state_id)

  let determinise_extchoice_item ctx (ExternalChoiceItem (constr, name, cont)) =
    let state_id, d = Determinise.determinise ctx cont in
    ExternalChoiceItem (constr, name, Deterministic (state_id, d))

  let rec real_inp_merge_one :
      type a. StateHash.t -> a inp_ -> a extchoice_item -> a inp_ =
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
         (let s1 = role.call_obj s1 and s2 = role.call_obj s2 in
          real_inp_merge ctx (Lazy.force s1) (Lazy.force s2))

  let inp_determinise role ctx = function
    | [ s ] ->
        role.make_obj
          (lazy
            (List.map
               (determinise_extchoice_item ctx)
               (Lazy.force (role.call_obj s))))
    | s :: ss -> List.fold_left (inp_merge role ctx) s ss
    | [] -> failwith "impossible: inp_determinise"

  let inp_force role ctx s =
    let s = role.call_obj s in
    List.iter
      (fun (ExternalChoiceItem (_, n, s)) ->
        ignore (Name.finalise n);
        force_determinised ctx s)
      (Lazy.force s)

  let inp_to_string role ctx s =
    role.method_name
    ^ "?{"
    ^ (let s = role.call_obj s in
       if Lazy.is_val s then
         String.concat ","
           (List.map
              (fun (ExternalChoiceItem (constr, _, cont)) ->
                constr.constr_name ^ "." ^ to_string ctx cont)
              (Lazy.force s))
       else "<lazy_inp>")
    ^ "}"
end

let out role lab name s =
  Deterministic
    ( StateHash.make_key (),
      Lazy.from_val
        {
          head = role.make_obj @@ lab.make_obj @@ Out (name, Lazy.from_val s);
          determinise_list = OutMerge.out_determinise role lab;
          force_determinised = OutMerge.out_force role lab;
          to_string = OutMerge.out_to_string role lab;
        } )

let inp role constr name s =
  Deterministic
    ( StateHash.make_key (),
      Lazy.from_val
        {
          head = role.make_obj @@ lazy [ ExternalChoiceItem (constr, name, s) ];
          determinise_list = InpMerge.inp_determinise role;
          force_determinised = InpMerge.inp_force role;
          to_string = InpMerge.inp_to_string role;
        } )

let unit =
  Deterministic
    ( StateHash.make_key (),
      Lazy.from_val
        {
          head = ();
          determinise_list = (fun _ _ -> ());
          force_determinised = (fun _ _ -> ());
          to_string = (fun _ _ -> "end");
        } )

let to_string seq = to_string (StateHash.make ()) seq

let determinise t =
  let _, h = Determinise.determinise (StateHash.make ()) t in
  let h = Lazy.force h in
  h.force_determinised (StateHash.make ()) h.head;
  h.head
