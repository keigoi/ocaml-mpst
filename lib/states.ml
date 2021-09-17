open Types
open StateHash
open Names

type 'a mergefun = 'a -> 'a -> 'a
type 'a mergenextfun = dict -> 'a -> unit

(* non-deterministic state *)
type 'a state = 'a nondet ref
and 'a nondet =
  | Determinised of 'a state_id * 'a head
    (** Deterministic transition  *)
  | Epsilon of 'a state list
    (** Epsilon transition (session merging) *)
  | InternalChoice : 'lr state_id * 'l state * 'r state * ('lr, 'l, 'r) disj -> 'lr nondet
    (** Internal choice *)
  | Unbound

exception UnguardedLoop of string

let fail_unguarded msg = raise (UnguardedLoop msg)

let merge_heads hds =
  let merge_head : 'a. 'a head -> 'a head -> 'a head = fun dl dr ->
    let d' = dl.merge dl.head dr.head in
    {head = d';merge = dl.merge; merge_next=dl.merge_next}
  in  
  List.fold_left merge_head (List.hd hds) (List.tl hds)

type 'a merged_or_backward_epsilon =
  ('a state_id * 'a head list, 'a state list) Either.t

let rec mem_phys k = function
  | x::xs -> k==x || mem_phys k xs
  | [] -> false

let rec epsilon_closure : 'a. 'a state -> 'a state_id * 'a head list =
  let ret_merged x = Either.Left x 
  and ret_backward_epsilon x = Either.Right x 
  in
  let rec loop 
    : 'a. visited:'a state list -> 'a state -> 'a merged_or_backward_epsilon =
    fun ~visited st ->
    if mem_phys st visited then
      ret_backward_epsilon [st]
    else
      match !st with
      | Determinised (sid,v) ->
        ret_merged (sid, [v])
      | Epsilon sts ->
        (* epsilon transitions: compute epsilon-closure *)
        let hds, cycles = 
          List.partition_map (loop ~visited:(st::visited)) sts 
        in
        if List.length hds > 0 then
          (* concrete transitons found - return the merged state ==== *)
          let ids, hds = List.split hds in
          let id = List.fold_left union_keys (List.hd ids) (List.tl ids) in
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
        and _idr, hrs = epsilon_closure tr
        in 
        let hl = merge_heads hls
        and hr = merge_heads hrs
        in
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
        let det = {head=tlr; merge; merge_next} in
        ret_merged (sid, [det])
    | Unbound -> 
      fail_unguarded "epsilon_closure: unguarded loop: Unbound"
    in
    fun st ->
    begin match loop ~visited:[] st with
    | Left (sid, hds) ->
      (sid, hds)
    | Right _ ->
      fail_unguarded "epsilon_closure: unguarded"
    end

let determinise_heads ~dict sid hds =
  begin match lookup dict sid with
  | Some hd -> hd
  | None -> 
    let hd = merge_heads hds in
    hd.merge_next (B(sid,hd)::dict) hd.head;
    hd
  end

let determinise : 'a. dict:dict -> 'a state -> 'a =
  fun ~dict st ->
    let sid, hds = epsilon_closure st in
    let hd = determinise_heads ~dict sid hds in
    st := Determinised (sid, hd);
    hd.head

let determinised_ st =
  match st with
  | {contents=Determinised(_,{head;_})} ->
      head
  (* | s ->
    (determinise ~dict:StateHash.empty s).head *)
  | _ ->
    failwith "determinise_: not determinised -- possible bug in determinisation?"


let gen_state_id () =
  let w = StateHash.newkey () in
  (w, [KeyEx w])

let make_unbound_state : 'a. unit -> 'a state = fun () ->
  ref Unbound

let bind_state ~from ~to_ =
  match !to_ with
  | Unbound -> 
    to_ := !from
  | _ -> 
    failwith "bind_state: state already bound -- possible bug in determinisation?"

let make_state : 'a. 'a mergefun -> 'a mergenextfun -> 'a -> 'a state = 
  fun merge detfun body ->
    let state_id = gen_state_id () in
    let det = 
      {head=body; merge; merge_next=detfun} 
    in
    ref (Determinised (state_id, det))

let make_internal_choice : 'l 'r 'lr. 'l state -> 'r state -> ('lr,'l,'r) disj -> 'lr state = fun sl sr disj ->
  ref (InternalChoice(gen_state_id (), sl,sr,disj))

let merge_state : 'a. 'a state -> 'a state -> 'a state = fun sl sr ->
  ref (Epsilon [sl; sr])

type 'var wrapped_head = 
  | WrappedHead : ('var,'s) constr * unit name * 's state_id * 's head -> 'var wrapped_head

type 'var wrapped_head_lazy = 
  | WrappedHeadLazy : ('var,'s) constr * unit name * ('s state_id * 's head list) lazy_t -> 'var wrapped_head_lazy
  
type 'var wrapped_nondet = 
  | WrappedDeterminised : 'var wrapped_head list -> 'var wrapped_nondet
  | WrappedNondet : 'var wrapped_head_lazy list -> 'var wrapped_nondet

and 'var wrapped_state = 'var wrapped_nondet ref

let make_wrapped = fun var n st ->
  ref @@ WrappedNondet([WrappedHeadLazy(var,n,lazy (epsilon_closure st))])
 
let rec classify = fun op ws ->
  match ws with
  | w1::ws -> 
    let w, ws = List.fold_left (fun (w1,ws) w2 -> 
      if w1==w2 then
        (w1, ws)
      else
        match op w1 w2 with
        | None -> 
          (w1, w2::ws)
        | Some w12 -> 
          (w12, ws)
      ) (w1, []) ws
    in
    let ws = classify op ws in
    w::ws
  | [] ->
    []

let cast_if_constrs_are_same : ('var,'a) constr -> ('var,'b) constr -> 'b -> 'a option =
  fun var1 var2 b ->
    var1.match_var (var2.make_var b)

let concat_heads_if_constrs_are_same : 'a. 'a wrapped_head_lazy -> 'a wrapped_head_lazy -> 'a wrapped_head_lazy option =
  let do_classify newid (var1,n1,_id1,hs1) (var2,n2,_id2,hs2) =
    let vs2 = List.map (fun x -> x.head) hs2 in
    begin match List.filter_map (cast_if_constrs_are_same var1 var2) vs2 with
    | (_::_) as vs2 -> 
      (* two constructors are same! *)          
      (* unify channel names *)
      unify_name n1 n2;
      (* wrap them into heads again *)
      let {merge;merge_next;_} = List.hd hs1 in
      let hs2 = List.map (fun v -> {head=v; merge; merge_next}) vs2 in
      (* and concatenate for later merging *)
      Some(WrappedHeadLazy(var1, n1, Lazy.from_val (newid, hs1@hs2)))
    | [] ->
      None
    end
  in
  fun (WrappedHeadLazy(var1,n1,lazy (id1,hs1))) (WrappedHeadLazy(var2,n2,lazy (id2,hs2))) ->
    match union_keys_generalised id1 id2 with
    | Left newid -> do_classify newid (var1,n1,id1,hs1) (var2,n2,id2,hs2)
    | Right newid -> do_classify newid (var2,n2,id2,hs2) (var1,n1,id1,hs1)

let classify_wrapped_names ws = classify concat_heads_if_constrs_are_same ws

let wrapped_heads : 'a wrapped_nondet -> 'a wrapped_head_lazy list = fun ws -> 
  match ws with
    | WrappedDeterminised(ws) -> 
      List.map (fun (WrappedHead(var,n,id,h)) -> WrappedHeadLazy(var,n,Lazy.from_val (id,[h]))) ws
    | WrappedNondet(ws) ->
      ws

let merge_wrapped_states wl wr =
  let wl' = wrapped_heads !wl
  and wr' = wrapped_heads !wr
  in
  ref (WrappedNondet(classify_wrapped_names (wl' @ wr')))

let determinise_wrapped ~dict r =
  match !r with
  | WrappedNondet(ws) ->
    let ws = 
      List.map (fun (WrappedHeadLazy(var,n,lazy (id,hds))) -> 
        let hd = determinise_heads ~dict id hds in
        WrappedHead(var,n,id,hd)) ws 
      in
      r := WrappedDeterminised ws
  | WrappedDeterminised(_) ->
    ()

let make_event_from_determinised_ ws =
  match ws with
  | {contents=WrappedDeterminised(ws)} ->
    let make_event (WrappedHead(var,n,_,hd)) =
      Event.wrap (Event.receive (Names.finalise_names n)) (fun () -> var.make_var hd.head)
    in
    Event.choose (List.map make_event ws)
  (* | w ->
    let ws = determinise_wrapped ~dict:StateHash.empty w in
    Event.sync @@ Event.choose (List.map make_event ws) *)
  | _ ->
    failwith "determinise_wrapped: not determinised -- possible bug in determinisation?"
