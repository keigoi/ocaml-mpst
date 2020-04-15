open Base

type 't local = 't Lin.gen Mergeable.t

type 'a elem =
    One : 'a local -> 'a one elem
  | List : 'a local list -> 'a list elem
  | Lazy : 'a elem lazy_t list -> 'a elem

(* arbitrary-length, mergeable tuple *)
type _ t =
  (* hidden *)
  | SeqCons : 'hd elem * 'tl t -> [`cons of 'hd  * 'tl] t
  | SeqRepeat : int * (int -> 'a local) -> ([`cons of 'a one * 'tl] as 'tl) t
  | SeqRecVars : 'a t lazy_t list -> 'a t
  | SeqBottom : 'a t

let fold_left0 f = function
  | [] -> assert false
  | x::xs -> List.fold_left f x xs

let rec from_one : type t. t one elem -> t local = function
  | One t -> t
  | Lazy es ->
    let ts = List.map (fun e -> Mergeable.make_recvar (lazy (from_one @@ Lazy.force e))) es in
    fold_left0 Mergeable.merge ts

let rec from_list : type t. size:int -> t list elem -> t local list = fun ~size -> function
  | List ts ->
    ts
  | Lazy es ->
    let make_recvar e i = Mergeable.make_recvar (lazy (List.nth (from_list ~size (Lazy.force e)) i)) in
    let es = List.map (fun e -> List.init size (make_recvar e)) es in
    fold_left0 (List.map2 Mergeable.merge) es

let merge_elem : type t. t elem -> t elem -> t elem = fun el er ->
  match el, er with
  | One x, One y -> One (Mergeable.merge x y)
  | List xs, List ys -> List (List.map2 Mergeable.merge xs ys)
  | (Lazy _ as e), One x ->
    One (Mergeable.merge x (from_one e))      
  | One x, (Lazy _ as e) ->
    One (Mergeable.merge (from_one e) x)      
  | (Lazy _ as e), List xs ->
    let size = List.length xs in
    List (List.map2 Mergeable.merge xs (from_list ~size e))
  | List xs, (Lazy _ as e) ->
    let size = List.length xs in
    List (List.map2 Mergeable.merge (from_list ~size e) xs)
  | Lazy e1, Lazy e2 ->
    Lazy (e1 @ e2)

let rec force_elem : type t. t elem -> unit = function
  | One t -> ignore (Mergeable.resolve t)
  | List ts -> List.iter (fun x -> ignore (Mergeable.resolve x)) ts
  | Lazy es ->
    let e = fold_left0 merge_elem (List.map Lazy.force es) in
    force_elem e

exception UnguardedLoopSeq

let recvar l = SeqRecVars [l]
let repeat i f = SeqRepeat(i,f)

let rec seq_head : type hd tl. [`cons of hd * tl] t -> hd elem =
  function
  | SeqCons(hd,_) -> hd
  | SeqRepeat(i,f) -> One(f i)
  | SeqRecVars (d::ds) -> List.fold_left merge_elem (seqvar_head d) (List.map seqvar_head ds)
  | SeqRecVars [] -> assert false
  | SeqBottom -> raise UnguardedLoopSeq
and seqvar_head : type hd tl. [`cons of hd * tl] t lazy_t -> hd elem = fun d ->
  Lazy [lazy (seq_head (Lazy.force d))]

let rec seq_tail : type hd tl. [`cons of hd * tl] t -> tl t =
  function
  | SeqCons(_,tl) -> tl
  | SeqRecVars ds -> SeqRecVars(List.map seqvar_tail ds)
  | SeqRepeat(i,f) -> SeqRepeat(i+1,f)
  | SeqBottom -> raise UnguardedLoopSeq
and seqvar_tail : type hd tl. [`cons of hd * tl] t lazy_t -> tl t lazy_t = fun d ->
  lazy (seq_tail (Lazy.force d))

let rec get : type a b xs ys. (a one, b, xs, ys) idx -> xs t -> a local = fun ln xs ->
  match ln with
  | Zero -> from_one @@ seq_head xs
  | Succ ln' -> get ln' (seq_tail xs)

let rec get_list : type a b xs ys. size:int -> (a list, b, xs, ys) idx -> xs t -> a local list = fun ~size ln xs ->
  match ln with
  | Zero -> from_list ~size @@ seq_head xs
  | Succ ln' -> get_list ~size ln' (seq_tail xs)

let rec put : type a b xs ys. (a one,b one,xs,ys) idx -> xs t -> b local -> ys t =
  fun ln xs b ->
  match ln with
  | Zero -> SeqCons(One b, seq_tail xs)
  | Succ ln' -> SeqCons(seq_head xs, put ln' (seq_tail xs) b)

let rec put_list : type a b xs ys. (a list,b list,xs,ys) idx -> xs t -> b local list -> ys t =
  fun ln xs bs ->
  match ln with
  | Zero -> SeqCons(List bs, seq_tail xs)
  | Succ ln' -> SeqCons(seq_head xs, put_list ln' (seq_tail xs) bs)

let rec seq_merge : type x. x t -> x t -> x t = fun l r ->
  match l,r with
  | SeqCons(_,_), _ ->
     let hd = merge_elem (seq_head l) (seq_head r) in
     let tl = seq_merge (seq_tail l) (seq_tail r) in
     SeqCons(hd, tl)
  | _, SeqCons(_,_) -> seq_merge r l
  (* delayed constructors are left as-is *)
  | SeqRecVars(us1), SeqRecVars(us2) -> SeqRecVars(us1 @ us2)
  (* repeat *)
  | SeqRepeat(i,f), _ -> SeqRepeat(i,f)
  | _, SeqRepeat(i,f) -> SeqRepeat(i,f)
  (* bottom *)
  | SeqBottom,_  -> SeqBottom
  | _, SeqBottom -> SeqBottom

let rec force_recvar : type x. x t lazy_t list -> x t lazy_t -> x t =
  fun hist w ->
  if Base.find_physeq hist w then begin
      raise UnguardedLoopSeq
    end else begin
      match Lazy.force w with
      | SeqRecVars [w'] -> force_recvar (w::hist) w'
      | s -> s
    end

let rec resolve_merge : type x. x t -> x t =
  function
  | SeqCons(hd,tl) ->
     let tl =
       try
         resolve_merge tl
       with
         UnguardedLoopSeq ->
         (* we do not raise exception here;
          * in recursion, an unguarded loop will occur in the last part of the sequence.
          * when one tries to take head/tail of SeqBottom, an exception will be raised.
          *)
         SeqBottom
     in
     SeqCons(hd, tl)
  | SeqRecVars [] -> assert false
  | SeqRecVars ((d::ds) as dss) ->
     resolve_merge
       (List.fold_left seq_merge (force_recvar dss d) (List.map (force_recvar dss) ds))
  | SeqRepeat(i,f) -> SeqRepeat(i,f)
  | SeqBottom -> SeqBottom

let rec force_all : type x. x t -> unit = function
  | SeqCons(hd,tl) ->
     force_elem hd;
     force_all tl
  | SeqRecVars [] -> assert false
  | SeqRecVars ((d::ds) as dss) ->
     force_all
       (List.fold_left seq_merge (force_recvar dss d) (List.map (force_recvar dss) ds))
  | SeqRepeat(_,_) -> ()
  | SeqBottom -> ()

let seq_merge_all : type x. x t list -> x t = function
  | [] -> failwith "seq_merge_all"
  | s::ss -> List.fold_left seq_merge s ss

let rec effective_length : type x. x t -> int = function
  | SeqRepeat(_,_) -> 0
  | SeqBottom -> 0
  | SeqCons(_,_) as s -> 1 + effective_length (seq_tail s)
  | SeqRecVars ds -> effective_length (seq_merge_all (List.map Lazy.force ds))
