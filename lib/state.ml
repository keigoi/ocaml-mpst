type 'a head = 'a StateHash.head
type 'a state_id = 'a StateHash.state_id
type cache = StateHash.dict
type 'a mergefun = 'a -> 'a -> 'a
type 'a mergenextfun = cache -> 'a -> unit

(* non-deterministic state *)
type 'a state = 'a nondet ref

and 'a nondet =
  | Determinised of 'a state_id * 'a head  (** Deterministic transition *)
  | Epsilon of 'a state list  (** Epsilon transition (session merging) *)
  | InternalChoice :
      'lr state_id * 'l state * 'r state * ('lr, 'l, 'r) Types.disj
      -> 'lr nondet  (** Internal choice *)
  | Unbound

exception UnguardedLoop of string

let fail_unguarded msg = raise (UnguardedLoop msg)

let merge_heads hds =
  let merge_head : 'a. 'a head -> 'a head -> 'a head =
   fun dl dr ->
    let d' = dl.merge dl.head dr.head in
    { head = d'; merge = dl.merge; merge_next = dl.merge_next }
  in
  List.fold_left merge_head (List.hd hds) (List.tl hds)

type 'a merged_or_backward_epsilon =
  ('a state_id * 'a head list, 'a state list) Either.t

let rec mem_phys k = function x :: xs -> k == x || mem_phys k xs | [] -> false

let rec epsilon_closure : 'a. 'a state -> 'a state_id * 'a head list =
  let ret_merged x = Either.Left x
  and ret_backward_epsilon x = Either.Right x in
  let rec loop
            : 'a.
              visited:'a state list -> 'a state -> 'a merged_or_backward_epsilon
      =
   fun ~visited st ->
    if mem_phys st visited then ret_backward_epsilon [ st ]
    else
      match !st with
      | Determinised (sid, v) -> ret_merged (sid, [ v ])
      | Epsilon sts ->
          (* epsilon transitions: compute epsilon-closure *)
          let hds, cycles =
            List.partition_map (loop ~visited:(st :: visited)) sts
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
            let cycles = List.concat cycles in
            let cycles =
              (* filter out epsilons pointing to myself *)
              List.filter (fun id -> mem_phys id visited) cycles
            in
            if List.length cycles > 0 then
              (* there're backward epsilons yet -- return it *)
              ret_backward_epsilon cycles
            else
              (* no backward epsilons anymore: unguarded recursion! *)
              fail_unguarded "epsilon_closure: unguarded"
      | InternalChoice (sid, tl, tr, disj) ->
          let _idl, hls = epsilon_closure tl
          and _idr, hrs = epsilon_closure tr in
          let hl = merge_heads hls and hr = merge_heads hrs in
          let tlr = disj.disj_concat hl.head hr.head in
          let merge lr1 lr2 =
            (* merge splitted ones *)
            let l = hl.merge (disj.disj_splitL lr1) (disj.disj_splitL lr2) in
            let r = hr.merge (disj.disj_splitR lr1) (disj.disj_splitR lr2) in
            (* then type-concatenate it *)
            disj.disj_concat l r
          in
          let merge_next ctx lr =
            hl.merge_next ctx (disj.disj_splitL lr);
            hr.merge_next ctx (disj.disj_splitR lr)
          in
          let det = { StateHash.head = tlr; merge; merge_next } in
          ret_merged (sid, [ det ])
      | Unbound -> fail_unguarded "epsilon_closure: unguarded loop: Unbound"
  in
  fun st ->
    match loop ~visited:[] st with
    | Left (sid, hds) -> (sid, hds)
    | Right _ -> fail_unguarded "epsilon_closure: unguarded"

let determinise_heads ~cache sid hds =
  match StateHash.lookup cache sid with
  | Some hd -> hd
  | None ->
      let hd = merge_heads hds in
      hd.merge_next (StateHash.add_binding sid hd cache) hd.head;
      hd

let determinise : 'a. cache:cache -> 'a state -> 'a =
 fun ~cache st ->
  let sid, hds = epsilon_closure st in
  let hd = determinise_heads ~cache sid hds in
  st := Determinised (sid, hd);
  hd.head

let determinised_ st =
  match st with
  | { contents = Determinised (_, { head; _ }) } -> head
  | _ ->
      failwith
        "determinise_: not determinised -- possible bug in determinisation?"

let gen_state_id () = StateHash.gen_state_id ()
let make_unbound : 'a. unit -> 'a state = fun () -> ref Unbound

let bind_state ~from ~to_ =
  match !to_ with
  | Unbound -> to_ := !from
  | _ ->
      failwith
        "bind_state: state already bound -- possible bug in determinisation?"

let make : 'a. 'a mergefun -> 'a mergenextfun -> 'a -> 'a state =
 fun merge detfun body ->
  let state_id = gen_state_id () in
  let det = { StateHash.head = body; merge; merge_next = detfun } in
  ref (Determinised (state_id, det))

let make_internal_choice :
      'l 'r 'lr. 'l state -> 'r state -> ('lr, 'l, 'r) Types.disj -> 'lr state =
 fun sl sr disj -> ref (InternalChoice (gen_state_id (), sl, sr, disj))

let merge : 'a. 'a state -> 'a state -> 'a state =
 fun sl sr -> ref (Epsilon [ sl; sr ])

let unit = make (fun _ _ -> ()) (fun _ _ -> ()) ()

type 'a t = 'a state
type 'a id = 'a state_id
