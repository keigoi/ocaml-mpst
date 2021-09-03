open Must
open Must.Types
open Must.StateHash

type 'a mergefun = 'a -> 'a -> 'a
type 'a detfun = context -> 'a -> unit

(* non-deterministic state *)
type 'a state = ('a state_id * 'a nondet) ref
and 'a nondet =
  | Determinised of 'a head
    (** Deterministic transition  *)
  | Epsilon of 'a state list
    (** Epsilon transition (session merging) *)
  | Concat : 'l state * 'r state * ('lr, 'l, 'r) disj -> 'lr nondet
    (** Internal choice (lazy) *)
  | Unbound

type 'a loop_or_success = ('a state list, ('a state_id * 'a head) * dict) Either.t
exception UnguardedLoop of string

let fail_unguarded msg = raise (UnguardedLoop msg)

let merge_session : 'a. 'a head -> 'a head -> 'a head = fun dl dr ->
  let d' = dl.merge dl.head dr.head in
  {head = d';merge = dl.merge; determinise_next=dl.determinise_next}

let rec determinise_head
: 'a. seen:'a state_id list -> dict:dict -> 'a state -> 'a loop_or_success
= fun ~seen ~dict st ->
  let old_sid = fst !st in
  match snd !st with
  | Determinised v -> 
    Right((old_sid, v), StateHash.empty)
  | Epsilon nds ->
    if List.mem old_sid seen then
      (* loop detection *)
      Left [st]
    else
      (* add current state to the loop set *)
      (* determinise states with outgoing epsilon transitions *)
      let cycles, success = List.partition_map (determinise_head ~seen:(old_sid::seen) ~dict) nds in
      let cycles = List.concat cycles in
      if List.length success = 0 then
        if List.exists (fun st -> List.mem (fst !st) seen) cycles then
          Left (st::cycles)
        else
          fail_unguarded "determinise_first: unguarded loop: Epsilon"
      else
        let states, new_dicts = List.split success in
        let sids, dets = List.split states in
        let new_dict = List.fold_left StateHash.union dict new_dicts in
        (* compute the new (power-)state id *)
        let new_state_id = List.fold_left union_keys (List.hd sids) (List.tl sids) in
        (* compute the new state *)
        let det =
          match lookup new_dict new_state_id with
          | Some det -> 
            (* already removed *)
            det
          | None -> 
            (* merge all *)
            List.fold_left merge_session (List.hd dets) (List.tl dets)
        in
        (* add new state to dict *)
        let new_dict = StateHash.union (mkbind new_state_id det) new_dict in
        (* overwrite the current state *)
        st := (new_state_id, Determinised det);
        (* collapse all epsilon loops *)
        List.iter (fun nd -> nd := new_state_id, Determinised det) cycles;
        Right((new_state_id, det), new_dict)
  | Concat(sl,sr,disj) ->
      (* remove epsilons in each branch *)
      begin match determinise_head ~seen:[] ~dict sl, determinise_head ~seen:[] ~dict sr with
      | Right((_, tl), dict1), Right((_,tr), dict2) ->
        (* then concatenate it *)
        let tlr = disj.disj_concat tl.head tr.head in
        let merge lr1 lr2 =
          (* merge splitted ones *)
          let l = tl.merge (disj.disj_splitL lr1) (disj.disj_splitL lr2) in
          let r = tr.merge (disj.disj_splitR lr1) (disj.disj_splitR lr2) in
          (* then concatenate it *)
          disj.disj_concat l r
        in
        let determinise_next ctx lr =
          tl.determinise_next ctx (disj.disj_splitL lr);
          tr.determinise_next ctx (disj.disj_splitR lr)
        in
        let det = {head=tlr; merge; determinise_next} in
        let new_dict = StateHash.union dict1 dict2 in
        st := (old_sid, Determinised det);
        Right((old_sid, det), new_dict)
      | _ -> 
        fail_unguarded "determinise_first: unguarded loop: Concat"
      end
  | Unbound ->
    fail_unguarded "determinise_first: unguarded loop: Unbound"

let determinise : 'a. context:context -> 'a state -> 'a head = 
  fun ~context nd ->
  match determinise_head ~seen:[] ~dict:context.dict nd with
  | Right((_,det), dict') ->
    let sid = fst !nd in
    begin if not (List.mem (V sid) context.visited) then
      det.determinise_next 
        {visited=(V(sid)::context.visited); dict=StateHash.union dict' context.dict}
        det.head
    end;
    det
  | Left(_) ->
    fail_unguarded "determinise: unguarded loop"

let gen_state_id () =
  let w = StateHash.newkey () in
  (w, [KeyEx w])

let make_unbound_state : 'a. unit -> 'a state = fun () ->
  ref (gen_state_id (), Unbound)

let make_determinised_state : 'a. ?state_id:'a state_id -> 'a mergefun -> 'a detfun -> 'a -> 'a state = 
  fun ?(state_id=gen_state_id ()) merge detfun body ->
  let det = 
    {head=body; merge; determinise_next=detfun} 
  in
  ref (state_id, Determinised det)

let merge_state : 'a. 'a state -> 'a state -> 'a state = fun sl sr ->
  ref (gen_state_id (), Epsilon [sl; sr])

let concat_state : 'l 'r 'lr. 'l state -> 'r state -> ('lr,'l,'r) disj -> 'lr state = fun sl sr disj ->
  ref (gen_state_id (), Concat(sl,sr,disj))
  
type _ t =
  | SeqCons : 'hd state * 'tl t -> [`cons of 'hd  * 'tl] t
  | SeqNil : ([` cons of unit * 'a] as 'a) t

let closed = make_determinised_state (fun _ _ -> ()) (fun _ _ -> ()) ()

let seq_head : type hd tl. [`cons of hd * tl] t -> hd state =
  function
  | SeqCons(hd,_) -> hd
  | SeqNil -> closed

let seq_tail : type hd tl. [`cons of hd * tl] t -> tl t =
  function
  | SeqCons(_,tl) -> tl
  | SeqNil -> SeqNil

let rec get : type a b xs ys. (a, b, xs, ys) idx -> xs t -> a state = fun ln xs ->
  match ln with
  | Zero -> seq_head xs
  | Succ ln' -> get ln' (seq_tail xs)

let rec put : type a b xs ys. (a,b,xs,ys) idx -> xs t -> b state -> ys t =
  fun ln xs b ->
  match ln with
  | Zero -> SeqCons(b, seq_tail xs)
  | Succ ln' -> SeqCons(seq_head xs, put ln' (seq_tail xs) b)

type 'a name_ =
| Name of 'a Event.channel
| Link of 'a name
and 'a name = 'a name_ ref

let rec unify_name (n1:'a name) (n2:'a name) =
  if n1==n2 then 
    () 
  else
    match !n1,!n2 with
    | Name _, Name _ -> n2 := Link n1
    | Link n1, _ -> unify_name n1 n2
    | _, Link n2 -> unify_name n1 n2

type 'var wrapped_name = 
  WrappedName : ('var,'s) constr * unit name * 's state -> 'var wrapped_name

type 's out = 
  unit name * 's state

type 'a inp =
  'a wrapped_name list ref 
(* the use of reference cell is to 'delay' the merging of continuations;
   to merge continuations of the same label, we need to get value of type 'a determinised.
   this invokes further merging in some case, entering into infinite loop.
 *)

let merge_wrapped_body (dict:dict) state_id (var1,n1,t1) (var2,n2,t2) =
  match var1.match_var (var2.make_var t2.head) with
  | Some t2' -> 
    (* two constructors are same! *)
    (* merge the continuations *)
    let t = t1.merge t1.head t2' in
    (* unify channel names *)
    unify_name n1 n2;
    Some(WrappedName(var1, n1, make_determinised_state ~state_id t1.merge t1.determinise_next t), dict)
  | None ->
    None

let determinise_first_ ~dict st =
  match determinise_head ~seen:[] ~dict st with
  | Right((det,sid),dict) -> 
    det, sid, dict
  | Left _ -> 
    fail_unguarded "merge_wrapped_name: unguarded loop"

let merge_wrapped_name ~dict w1 w2 =
  match w1, w2 with
  | WrappedName(var1,n1,s1), WrappedName(var2,n2,s2) ->
    let sid1, t1, dict = determinise_first_ ~dict s1 in
    let sid2, t2, dict = determinise_first_ ~dict s2 in
    match union_keys_generalised sid1 sid2 with
    | Left sid_a -> merge_wrapped_body dict sid_a (var1,n1,t1) (var2,n2,t2)
    | Right sid_b ->  merge_wrapped_body dict sid_b (var2,n2,t2) (var1,n1,t1)
    
let rec merge_wrapped_names ~dict = function
  | w1::ws -> 
    let w, ws, dict = List.fold_left (fun (w1,ws,dict) w2 -> 
      if w1==w2 then
        (w1, ws, dict)
      else
        match merge_wrapped_name ~dict w1 w2 with
        | None -> 
          (w1, w2::ws, dict)
        | Some (w12, dict) -> 
          (w12, ws, dict)
      ) (w1, [], dict) ws
    in
    let ws, dict = merge_wrapped_names ~dict ws in
    w::ws, dict
  | [] ->
    [], dict

let merge_inp dst_role sl sr =
  let wl : 'a inp = dst_role.call_obj sl
  and wr : 'a inp = dst_role.call_obj sr
  in
  dst_role.make_obj (ref (!wl @ !wr))

let determinise_inp dst_role context s =
  let ws, dict = merge_wrapped_names ~dict:context.dict !(dst_role.call_obj s) in
  List.iter (fun (WrappedName(_,_,s)) -> ignore (determinise ~context:{context with dict} s)) ws

let merge_out dst_role labobj sl sr =
  let (nl,sl') = labobj.call_obj @@ dst_role.call_obj sl
  and (nr,sr') = labobj.call_obj @@ dst_role.call_obj sr 
  in
  unify_name nl nr;
  let s = merge_state sl' sr' in
  dst_role.make_obj @@ labobj.make_obj (nl, s)

let determinise_out dst_role labobj context s =
  let _, s = labobj.call_obj @@ dst_role.call_obj s in
  ignore @@ determinise ~context s

let rec finalise_names n1 =
  match !n1 with
  | Name ch -> ch
  | Link n1' -> 
    let ch = finalise_names n1' in
    n1 := Name ch;
    ch

let determinised_of_state_ = function
  | {contents=(_,Determinised{head=x;_})} ->
    x
  | _ -> assert false
  
let branch (ws:'a inp) =
  let make_event (WrappedName(var,n,s))= 
    Event.wrap (Event.receive (finalise_names n)) (fun () -> var.make_var (determinised_of_state_ s))
  in
  Event.sync @@ Event.choose (List.map make_event !ws)

let select ((n,s):_ out) =
  Event.sync (Event.send (finalise_names n) ());
  determinised_of_state_ s

let close () = ()

let (-->) ri rj lab cont =
  let name = ref @@ Name (Event.new_channel ()) in
  let tj = get rj.role_index cont in
  let tj' = 
    ri.role_label.make_obj @@ (ref [WrappedName (lab.var, name, tj)] : _ inp)
  in
  let tj' =
    make_determinised_state
      (merge_inp ri.role_label)
      (determinise_inp ri.role_label)
      tj'
  in
  let mid = put rj.role_index cont tj' in
  let ti = get ri.role_index mid in
  let ti' =
    rj.role_label.make_obj @@
      lab.obj.make_obj ((name, ti): _ out)
  in
  let ti' =
    make_determinised_state 
      (merge_out rj.role_label lab.obj)
      (determinise_out rj.role_label lab.obj)
      ti'
  in
  let curr = put ri.role_index mid ti' in
  curr

let rec seq_merge : type x. x t -> x t -> x t = fun l r ->
  match l,r with
  | SeqCons(_,_), _ ->
    let hd = merge_state (seq_head l) (seq_head r) in
    let tl = seq_merge (seq_tail l) (seq_tail r) in
    SeqCons(hd, tl)
  | _, SeqCons(_,_) -> seq_merge r l
  | SeqNil, SeqNil -> SeqNil

let choice_at r disj (r', left) (r'', right) =
  let sl = get r'.role_index left
  and sr = get r''.role_index right
  and left = put r'.role_index left closed
  and right = put r''.role_index right closed
  in
  let mid = seq_merge left right in
  put r.role_index mid (concat_state sl sr disj)

(* let comm_opt ri rj lab disj (rj',cont1) (rj'',cont2) =
  let sjl = get rj'.role_index cont1
  and sjr = get rj''.role_index cont2
  and cont1 = put rj'.role_index cont1 closed
  and cont2 = put rj''.role_index cont2 closed
  in
  let mid = seq_merge cont1 cont2 in
  let mid' = put rj.role_index mid (concat_state sjl sjr disj) in
  let si = get ri.role_index mid' in
  put ri.role_index mid' si *)

type (_,_) upd =
| [] : ('aa, 'aa) upd
| (::) : ('a,'b,'aa,'bb,_,_) role * ('bb,'cc) upd -> ('aa,'cc) upd

(** shadowing upd above *)
type 'a list = 'a Stdlib.List.t =
| [] 
| (::) of 'a * 'a list

let rec put_unbound : type aa bb. aa t -> (aa,bb) upd -> bb t =
  fun seq upd ->
  match upd with
  | [] -> 
    seq
  | role::upd -> 
    put_unbound (put role.role_index seq (make_unbound_state ())) upd

let finish = SeqNil

let rec tying_unbound : type u. u t -> u t -> unit = fun self g ->
  match self with
  | SeqNil -> ()
  | SeqCons({contents=(_,Unbound)} as st,self) -> 
    st := !(seq_head g);
    tying_unbound self (seq_tail g)
  | SeqCons(_,self) -> 
    tying_unbound self (seq_tail g)

let fix_with upd f =
  let self = put_unbound SeqNil upd in
  let g = f self in
  tying_unbound self g;
  g

let rec extract : type u. u t -> u = function
  | SeqCons(st,tail) ->
    `cons((determinise ~context:{visited=[];dict=StateHash.empty} st).head, extract tail)
  | SeqNil ->
    let rec nil = `cons((),nil) in nil

module Util = struct
  let a = {role_label={make_obj=(fun v->object method role_A=v end);
                     call_obj=(fun o->o#role_A)};
         role_index=Zero}
  let b = {role_label={make_obj=(fun v->object method role_B=v end);
                      call_obj=(fun o->o#role_B)};
          role_index=Succ Zero}
  let c = {role_label={make_obj=(fun v->object method role_C=v end);
                      call_obj=(fun o->o#role_C)};
          role_index=Succ (Succ Zero)}
  let d = {role_label={make_obj=(fun v->object method role_D=v end);
                      call_obj=(fun o->o#role_D)};
          role_index=Succ (Succ (Succ Zero))}

  let msg =
    {obj={make_obj=(fun f -> object method msg=f end);
          call_obj=(fun o -> o#msg)};
    var={make_var=(fun v -> `msg(v));
          match_var=(function `msg(v) -> Some v | _ -> None)}}
  let left =
    {obj={make_obj=(fun f -> object method left=f end);
          call_obj=(fun o -> o#left)};
    var={make_var=(fun v -> `left(v));
          match_var=(function `left(v) -> Some v | _ -> None)}}
  let right =
    {obj={make_obj=(fun f -> object method right=f end);
          call_obj=(fun o -> o#right)};
    var={make_var=(fun v -> `right(v));
          match_var=(function `right(v) -> Some v | _ -> None)}}
  let middle =
    {obj={make_obj=(fun f -> object method middle=f end);
          call_obj=(fun o -> o#middle)};
    var={make_var=(fun v -> `middle(v));
          match_var=(function `middle(v) -> Some v | _ -> None)}}
  let ping =
    {obj={make_obj=(fun f -> object method ping=f end);
          call_obj=(fun o -> o#ping)};
    var={make_var=(fun v -> `ping(v));
          match_var=(function `ping(v) -> Some v | _ -> None)}}
  let pong =
    {obj={make_obj=(fun f -> object method pong=f end);
          call_obj=(fun o -> o#pong)};
    var={make_var=(fun v -> `pong(v));
          match_var=(function `pong(v) -> Some v | _ -> None)}}
  let fini =
    {obj={make_obj=(fun f -> object method fini=f end);
          call_obj=(fun o -> o#fini)};
    var={make_var=(fun v -> `fini(v));
          match_var=(function `fini(v) -> Some v | _ -> None)}}

  let left_or_right =
    {disj_concat=(fun l r -> object method left=l#left method right=r#right end);
    disj_splitL=(fun lr -> (lr :> <left : _>));
    disj_splitR=(fun lr -> (lr :> <right : _>));
    }
  let right_or_left =
    {disj_concat=(fun l r -> object method right=l#right method left=r#left end);
    disj_splitL=(fun lr -> (lr :> <right : _>));
    disj_splitR=(fun lr -> (lr :> <left : _>));
    }


  let to_ m r1 r2 r3 =
    let (!) x = x.role_label in
    {disj_concat=(fun l r -> !r1.make_obj (m.disj_concat (!r2.call_obj l) (!r3.call_obj r)));
    disj_splitL=(fun lr -> !r2.make_obj (m.disj_splitL @@ !r1.call_obj lr));
    disj_splitR=(fun lr -> !r3.make_obj (m.disj_splitR @@ !r1.call_obj lr));
    }
  let to_a m = to_ m a a a
  let to_b m = to_ m b b b
  let to_c m = to_ m c c c
  let to_d m = to_ m d d d

  let left_middle_or_right =
    {disj_concat=(fun l r -> object method left=l#left method middle=l#middle method right=r#right end);
    disj_splitL=(fun lr -> (lr :> <left : _; middle: _>));
    disj_splitR=(fun lr -> (lr :> <right : _>));
    }

  let left_or_middle =
    {disj_concat=(fun l r -> object method left=l#left method middle=r#middle end);
    disj_splitL=(fun lr -> (lr :> <left : _>));
    disj_splitR=(fun lr -> (lr :> <middle : _>));
    }

  let left_or_middle_right =
    {disj_concat=(fun l r -> object method left=l#left method middle=r#middle method right=r#right end);
    disj_splitL=(fun lr -> (lr :> <left : _>));
    disj_splitR=(fun lr -> (lr :> <middle: _; right : _>));
    }

  let middle_or_right =
    {disj_concat=(fun l r -> object method middle=l#middle method right=r#right end);
    disj_splitL=(fun lr -> (lr :> <middle : _>));
    disj_splitR=(fun lr -> (lr :> <right : _>));
    }

end

open Util
let () =
  let expect_unguarded msg g =
    begin try
      ignore (extract g);
      failwith "unexpected: it should fail!"
    with
      (UnguardedLoop str) as _e -> 
        print_endline (str ^ " (expected: " ^ msg ^ ")");
        (* raise _e *)
    end;
  in
  let bottom () = fix_with [a;b;c] (fun t -> t) in
  let () =
    expect_unguarded "bottom" @@ bottom ()
  in
  let () =
    expect_unguarded "bottom after comm" @@ (a -->b) msg @@ bottom ()
  in
  let () =
    expect_unguarded "bottom after choice" @@
      choice_at a (to_b left_or_right)
      (a, (a --> b) left @@ (b --> c) middle @@ bottom ())
      (a, (a --> b) right @@ (b --> c) middle finish)
  in
  let () =
    expect_unguarded "bottom after choice 2" @@
      choice_at a (to_b left_or_right)
      (a, bottom ())
      (a, (a --> b) right @@ (b --> c) middle finish)
  in
  let () =
    expect_unguarded "bottom after choice 3" @@
      choice_at b (to_b left_or_right)
      (c, (c --> b) left @@ (b --> a) middle @@ bottom ())
      (c, (c --> b) right @@ (b --> a) middle finish)
  in
  let () =
    expect_unguarded "bottom after choice loop" @@
      fix_with [a;b;c] @@ fun t -> 
        choice_at a (to_b left_or_right)
        (a, (a --> b) left @@ (b --> c) middle t)
        (a, (a --> b) right @@ (b --> c) middle @@ bottom ())
  in
  let () =
    expect_unguarded "bottom after choice loop 2" @@
      fix_with [a;b;c] @@ fun t -> 
        choice_at c (to_b left_or_right)
        (c, (c --> b) left t)
        (c, (c --> b) right t)
  in
  let () =
    expect_unguarded "bottom after choice loop 3" @@
    choice_at b (to_a left_or_right)
      (b, (b --> a) left @@
          fix_with [a;b;c] @@ fun t -> 
          choice_at c (to_b left_or_right)
          (c, (c --> b) left t)
          (c, (c --> b) right t))
      (b, (b --> a) right @@
          fix_with [a;b;c] @@ fun t -> 
            choice_at c (to_b left_or_right)
            (c, (c --> b) left t)
            (c, (c --> b) right t))
  in
  let () =
    let cd = 
      fix_with [a;b;c;d] @@ fun t -> 
        choice_at c (to_d left_or_right)
        (c, (c --> d) left t)
        (c, (c --> d) right t)
    in
    expect_unguarded "bottom after choice loop 4" @@
    choice_at b (to_a left_or_right)
      (b, cd)
      (b, (b --> a) right cd)
  in
  let () =
    expect_unguarded "loop merge" @@
      choice_at a (to_b left_or_right)
      (a, (a --> b) left @@ 
          fix_with [a;b;c] (fun t -> 
            choice_at a (to_b left_or_right) 
            (a, (a --> b) left t)
            (a, (a --> b) right t)
          ))
      (a, (a --> b) right @@ (a --> b) left @@ (b --> c) left finish)
  in
  let _g =
    extract @@
      (a --> b) msg finish
  in
  let _g =
    extract @@ 
      choice_at a (to_b left_or_right)
      (a,(a --> b) left finish)
      (a,(a --> b) right finish)
  in
  let _g =
    extract @@
      fix_with [a;b] (fun t -> (a --> b) msg t)
  in
  let _g8 =
    extract @@
    fix_with [a;b;c] (fun t -> 
        choice_at a (to_b left_or_right)
        (a, (a --> b) left @@ (b --> c) middle t)
        (a, (a --> b) right @@ (b --> c) middle t))
  in
  let _g7 =
    extract @@
    fix_with [a;b] (fun t -> 
      (a --> b) left @@
      fix_with [a;b] (fun u -> 
        choice_at a (to_b left_or_right)
        (a, t)
        (a, (a --> b) right @@ u)
        ))
  in
  let () =
    let `cons((sa:'sa7),`cons((sb:'sb7),_)) = _g7 in
    let _ta = Thread.create (fun () -> 
        let rec loop sa i =
          if i < 5 then
            loop (select sa#role_B#right) (i+1)
          else if i < 10 then
            loop (select sa#role_B#left) (i+1)
          else
            ()
        in
        loop (select sa#role_B#left) 0
      ) ()
    in
    let _tb = Thread.create (fun () -> 
        let rec loop (sb:'sb7) =
          match branch sb#role_A with
          | `left(sb) ->
            print_endline "g7: tb: left";
            loop sb
          | `right(sb) ->
            print_endline "g7: tb: right";
            loop sb
        in
        loop sb
        ) ()
    in
    Thread.join _ta
  in
  let _g6 =
    print_endline "g6 determinising";
    extract @@
      choice_at a (to_b left_or_right)
      (a, fix_with [a;b;c] (fun t -> (a --> b) left @@ (a --> c) msg t))
      (a, fix_with [a;b;c] (fun t -> (a --> b) right @@ (a --> c) msg t))
  in
  print_endline "g6 determinised";
  let () = 
    let `cons(_sa,`cons(_sb,`cons(_sc,_))) = _g6 in
    let _ta = Thread.create (fun () ->
      print_endline "ta start";
      let rec loop sa i =
        if i > 100 then
          ()
        else begin 
          if i mod 10 = 0 then begin
              Printf.printf "%d\n" i;
              flush stdout
          end;
          let sa = select (select (sa#role_B#left))#role_C#msg in
          loop sa (i+1)
        end
      in 
      loop (_sa :> (<role_B: <left: <role_C:<msg: 'a out> > out> > as 'a)) 0;
      print_endline "g6: ta finish"
      ) ()
    in
    let _tb = Thread.create (fun () ->
      print_endline "tb start";
      let rec loop sb i =
        if i mod 10 = 0 then begin
          Printf.printf "%d\n" i;
          flush stdout
        end;
        match branch sb#role_A with
        | `left sb -> 
          loop sb (i+1)
        | `right sb -> 
          loop sb (i+1)
      in
      loop _sb 0
      ) ()
    in
    let _tc = Thread.create (fun () -> 
        let rec loop sc =
          let `msg sc = branch sc#role_A in
          loop sc
        in 
        loop _sc
      ) () 
    in
    Thread.join _ta
  in
  let _g5 =
    extract @@
      fix_with [a;b;c] (fun t -> 
          choice_at a (to_b left_or_right)
          (a, (a-->b) left @@ (b-->c) msg t)
          (a, (a-->b) right @@ (b-->c) msg @@ (b-->c) left finish)
        )
  in
  let _g4 =
    extract @@
    fix_with [a;b;c] (fun t -> 
      choice_at a (to_b left_middle_or_right)
      (a, choice_at a (to_b left_or_middle)
          (a, (a --> b) left t)
          (a, (a --> b) middle t))
      (a, (a --> b) right @@ (a --> c) msg finish)
    )
  in
  let () =
    let `cons((sa:'ta),`cons((sb:'tb),`cons((sc:'tc),_))) = _g4 in
    let _t1 =
      Thread.create (fun () -> 
        let rec loop (sa:'ta) i =
          if i<5 then 
            loop (select sa#role_B#left) (i+1)
          else if i<10 then
            loop (select sa#role_B#middle) (i+1)
          else
            select (select sa#role_B#right)#role_C#msg
        in
        loop sa 0;
        print_endline "g4: ta finished"
      ) ()
    in
    let _t2 =
      Thread.create (fun () -> 
        let rec loop (sb:'tb) =
          match branch sb#role_A with
          | `left sb -> loop sb
          | `middle sb -> loop sb
          | `right sb -> close sb
        in 
        loop sb;
        print_endline "g4: tb finished"
      ) ()
    in
    let _t3 =
      Thread.create (fun () -> 
        let loop (sc:'tc) =
          let `msg sc = branch sc#role_A in
          close sc
        in 
        loop sc;
        print_endline "g4: tc finished"
      ) ()
    in
    List.iter Thread.join [_t1; _t2; _t3];
  in
  let _g3 =
    extract @@
    fix_with [a;b;c] (fun t -> 
      (a --> b) msg @@
      choice_at a (to_c left_or_right)
      (a, (a --> c) left @@ (c --> a) msg t)
      (a, (a --> c) right @@ (c --> a) msg t)
    )
  in
  let () =
    let `cons(sa,`cons(sb,`cons(sc,_))) = _g3 in
    let _t1 =
      Thread.create (fun () -> 
        let rec loop sa i =
          let sa = select sa#role_B#msg in
          if i<5 then 
            let `msg sa = branch (select sa#role_C#left)#role_C in
            loop sa (i+1)
          else if i<10 then
            let `msg sa = branch (select sa#role_C#right)#role_C in
            loop sa (i+1)
          else
            print_endline "g3: t1: stop"
        in
        loop sa 0
      ) ()
    in
    let _t2 =
      Thread.create (fun () -> 
        let rec loop sb =
          let `msg sb = branch sb#role_A in
          loop sb
        in loop sb
      ) ()
    in
    let _t3 =
      Thread.create (fun () -> 
        let rec loop sc =
          match branch sc#role_A with
          | `left sc -> loop (select sc#role_A#msg)
          | `right sc -> loop (select sc#role_A#msg)
        in loop sc
      ) ()
    in
    ()
  in
  let _g2 = 
    extract @@
    fix_with [a;b;c] (fun t ->
      (a --> b) left @@
      choice_at a (to_b left_or_right)
      (a, t)
      (a, (a --> b) right @@ (b --> c) right @@ finish)
    )
  in
  let () =
    let `cons(sa,`cons(sb, `cons(sc, _))) = _g2 in
    let _ta = Thread.create (fun () -> 
      let rec loop sa i =
        if i < 10 then
          loop (select sa#role_B#left) (i+1)
        else
          close (select sa#role_B#right)
      in
      loop (select sa#role_B#left) 0;
      print_endline "g2: ta finished"

      ) ()
    in
    let _tb = Thread.create (fun () -> 
        let rec loop sb =
          match branch sb#role_A with
          | `left sb -> loop sb
          | `right sb -> close (select sb#role_C#right)
        in
        loop sb;
        print_endline "g2: tb finished"
      ) ()
    in
    let `right sc = branch sc#role_B in
    close sc;
    print_endline "g2: tc finished"
  in  
  let _g1 =
    extract @@
    fix_with [a;b;c] (fun t ->
        choice_at a (to_b left_or_right)
        (a, (a --> b) left t)
        (a, (a --> b) right @@ (b --> c) right finish)
      )
  in
  let () =
    let `cons(sa,`cons(sb,`cons(sc,_))) = _g1 in
    let _ta = Thread.create (fun () ->
        let rec loop sa i =
          if i < 1000 then
            let sa = select sa#role_B#left in
            loop sa (i+1)
          else
            close (select sa#role_B#right)
          in
          loop sa 0;
          print_endline "g1: ta finish"
      ) () 
    in
    let _tb = Thread.create (fun () -> 
      let rec loop sb acc =
        match branch sb#role_A with
        | `left sb -> loop sb (acc+1)
        | `right sb -> 
          close (select sb#role_C#right);
          acc
      in
      Printf.printf "%d\n" @@ loop sb 0;
      flush stdout
      ) ()
    in
    let `right(sc) = branch sc#role_B in
    close sc
  in
  let _g0 =
    extract @@
      choice_at a (to_b left_or_right)
      (a, fix_with [a;b] (fun t -> (a --> b) left t))
      (a, fix_with [a;b] (fun t -> (a --> b) right t))
  in
  let () = 
    let `cons(sa,`cons(sb,_)) = _g0 in
    let ta = Thread.create (fun () ->
      print_endline "ta start";
      let rec f sa i =
        if i > 10000 then
          ()
        else begin 
            if i mod 1000 = 0 then 
              Printf.printf "%d\n" i;
              flush stdout;
          let sa = select (sa#role_B#left) in
          f sa (i+1)
        end
      in 
      f (sa :> (<role_B: <left: 'b out> > as 'b)) 0;
      print_endline "g0: ta finish"
      ) ()
    in
    let _tb = Thread.create (fun () ->
      print_endline "tb start";
      let rec f sb i =
        (* begin 
          if i mod 10 = 0 then 
            Printf.printf "%d\n" i;
        end; *)
        match branch sb#role_A with
        | `left sb -> 
          (* print_endline "tb: left"; *)
          f sb (i+1)
        | `right sb -> 
          (* print_endline "tb: right"; *)
          f sb (i+1)
      in
      f sb 0
      ) ()
    in
    Thread.join ta
  in
  print_endline "ok";
  ()

