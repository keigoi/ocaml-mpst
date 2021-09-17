open StateHash
open Names
open Types
open States

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
