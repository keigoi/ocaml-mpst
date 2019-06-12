
let map_option f = function
  | Some x -> Some (f x)
  | None -> None

let rec find_physeq : 'a. 'a list -> 'a -> bool = fun xs y ->
  match xs with
  | x::xs -> if x==y then true else find_physeq xs y
  | [] -> false

type ('lr, 'l, 'r) obj_merge =
  {obj_merge: 'l -> 'r -> 'lr;
   obj_splitL: 'lr -> 'l;
   obj_splitR: 'lr -> 'r;
  }

type ('la,'va) method_ =
  {make_obj: 'va -> 'la;
   call_obj: 'la -> 'va}

module Mergeable
(* : sig
 *   type 'a t
 *   val merge : 'a t -> 'a t -> 'a t
 *   val apply : ('a -> 'b) t -> 'a -> 'b t
 *   val no_merge : 'a -> 'a t
 *   val bare_ : ('a option -> 'a) -> 'a t
 *   val out : 'a t -> 'a option -> 'a
 *   val out_ : 'a t -> 'a
 *   val guarded : 'a t lazy_t list -> 'a t
 *   val obj : ('v -> 'v -> 'v) -> (< .. > as 'o, 'v) method_ -> 'v -> 'o t
 *   val objfun : ('v -> 'v -> 'v) -> (< .. > as 'o, 'v) method_ -> ('p -> 'v) -> ('p -> 'o) t
 *   exception UnguardedLoop
 * end *)
  = struct
  type 'a t =
    | Guarded_ of 'a t lazy_t list
    | Val_ of ('a option -> 'a)

  exception UnguardedLoop

  type 'a merge__ = 'a option -> 'a

  let merge0 : type x. x merge__ -> x merge__ -> x merge__ = fun l r ->
    fun obj -> l (Some (r obj))

  let merge : 'a. 'a t -> 'a t -> 'a t =
    fun l r ->
    match l, r with
    | Val_ ll, Val_ rr ->
       Val_ (merge0 ll rr)
    | Guarded_ g1, Guarded_ g2 ->
       Guarded_ (g1 @ g2)
    | ((Val_ _) as b, Guarded_ g)
    | (Guarded_ g, ((Val_ _) as b)) ->
       Guarded_ ([lazy b] @ g)

  let merge_all : 'a. 'a t list -> 'a t = function
    | [] -> failwith "merge_all: empty"
    | x::xs -> List.fold_left merge x xs

  let rec apply : 'a 'b. ('a -> 'b) t -> 'a -> 'b t = fun g v ->
    match g with
    | Guarded_(gs) ->
       Guarded_(List.map (fun g -> lazy (apply (Lazy.force g) v)) gs)
    | Val_ f ->
       Val_ (fun w ->
           match w with
           | Some w -> f (Some (fun _ -> w)) v
           | None -> f None v)

  let rec check_and_force hist t =
    match t with
    | (Val_ x) -> x
    | Guarded_ gs ->
       let solved =
         List.fold_left (fun acc g ->
             try
               if find_physeq hist g then begin
                   acc (* found a loop. drop it *)
                 end else begin
                   let g = check_and_force (g::hist) (Lazy.force g)
                   in g :: acc
                 end
             with
               UnguardedLoop -> acc)
           [] gs
       in
       match solved with
       | [] ->
          raise UnguardedLoop
       | x::xs ->
          List.fold_left merge0 x xs

  (* 
   * force: try to resolve choices which involve recursion variables.
   * this is called during get_ep (i.e. after finishing global combinators)
   *)
  let force : 'a. 'a t -> 'a t = fun t ->
    match t with
    | (Val_ _) ->
       t (* already evaluated *)
    | (Guarded_ [_]) ->
       t (* do not touch -- this is a recursion variable *)
    | (Guarded_ (_::_)) ->
       Val_ (check_and_force [] t) (* a choice which involves recursion variable -- try to resolve it *)
    | (Guarded_ []) ->
       t

  let out__ : 'a. 'a t -> 'a option -> 'a = fun t ->
    check_and_force [] t


  let out_ : 'a. 'a t -> 'a = fun g ->
    out__ g None

  let out : 'a. 'a t -> 'a option -> 'a = fun g ->
    out__ g
    
  let no_merge : 'a. 'a -> 'a t  = fun v ->
    Val_ (fun _ -> v)
    
  let bare_ : 'a. ('a option -> 'a) -> 'a t  = fun v ->
    Val_ v

  let make : 'v. ('v -> 'v -> 'v) -> 'v -> 'v t = fun merge v ->
    Val_
      (function
       | None -> v
       | Some v2 -> merge v v2)

  let guarded : 'a. 'a t lazy_t list -> 'a t  = fun v ->
    Guarded_ v

  let obj_
      : 'o 'v. (< .. > as 'o, 'v) method_ ->  ('v option -> 'v) -> 'o t =
    fun meth val_ ->
    bare_ (fun obj ->
        match obj with
        | None ->
           meth.make_obj (val_ None)
        | Some obj ->
           let val2 = meth.call_obj obj in
           meth.make_obj (val_ (Some val2)))

  let obj
      : 'o 'v. ('v -> 'v -> 'v) -> (< .. > as 'o, 'v) method_ -> 'v -> 'o t =
    fun merge meth val_ ->
    bare_ (fun obj ->
        match obj with
        | None ->
           meth.make_obj val_
        | Some obj ->
           let val2 = meth.call_obj obj in
           meth.make_obj (merge val_ val2))

  let objfun
      : 'o 'v 'p. ('v -> 'v -> 'v) -> (< .. > as 'o, 'v) method_ -> ('p -> 'v) -> ('p -> 'o) t =
    fun merge meth val_ ->
    bare_ (fun obj p ->
        match obj with
        | None ->
           meth.make_obj (val_ p)
        | Some obj ->
           let val2 = meth.call_obj (obj p) in
           meth.make_obj (merge (val_ p) val2))
end

type _ seq =
  | Seq : 'hd Mergeable.t * 'tl seq -> [`cons of 'hd * 'tl] seq
  | SeqAll : 'a Mergeable.t -> ([`cons of 'a * 'tl] as 'tl) seq
  | SeqGuard : 't seq lazy_t list -> 't seq
  | SeqError : 't seq

exception UnguardedLoopSeq

let rec seq_head : type hd tl. [`cons of hd * tl] seq -> hd Mergeable.t =
  function
  | Seq(hd,_) -> hd
  | SeqAll(a) -> a
  | SeqGuard xss ->
     let gs =
       List.map
         (fun xs -> Mergeable.guarded [lazy (seq_head (Lazy.force xs))])
         xss
     in
     List.fold_left Mergeable.merge (List.hd gs) (List.tl gs)
  | SeqError -> raise UnguardedLoopSeq

let rec seq_tail : type hd tl. [`cons of hd * tl] seq -> tl seq = fun xs ->
  match xs with
  | Seq(_,tl) -> tl
  | (SeqAll _) as s -> s
  | SeqGuard xss -> SeqGuard(List.map (fun xs -> lazy (seq_tail (Lazy.force xs))) xss)
  | SeqError -> raise UnguardedLoopSeq

let rec seq_merge : type t. t seq -> t seq -> t seq =
  fun ls rs ->
  match ls, rs with
  | Seq(hd_l,tl_l), Seq(hd_r, tl_r) ->
     Seq(Mergeable.merge hd_l hd_r, seq_merge tl_l tl_r)
  | SeqGuard(xss), Seq(hd_r, tl_r) ->
     let hd_l =
       Mergeable.guarded (List.map (fun xs -> lazy (seq_head (Lazy.force xs))) xss)
     in
     let tl_l = SeqGuard(List.map (fun xs -> lazy (seq_tail (Lazy.force xs))) xss) in
     Seq(Mergeable.merge hd_l hd_r, seq_merge tl_l tl_r)
  | Seq(hd_l, tl_l), SeqGuard(yss) ->
     let hd_r =
       Mergeable.guarded (List.map (fun ys -> lazy (seq_head (Lazy.force ys))) yss)
     in
     let tl_r = SeqGuard(List.map (fun ys -> lazy (seq_tail (Lazy.force ys))) yss) in
     Seq(Mergeable.merge hd_l hd_r, seq_merge tl_l tl_r)
  | _, SeqAll(a) -> SeqAll(a)
  | SeqAll(a), _ -> SeqAll(a)
  | SeqGuard(w1), SeqGuard(w2) -> SeqGuard(w1 @ w2)
  | SeqError,_  -> raise UnguardedLoopSeq
  | _, SeqError -> raise UnguardedLoopSeq

let rec remove_guards_seq : type t. t seq lazy_t list -> t seq lazy_t -> t seq =
  fun acc w ->
  if find_physeq acc w then begin
      raise UnguardedLoopSeq
    end else begin
      match Lazy.force w with
      | SeqGuard w' ->
         let ws = List.map (remove_guards_seq (w::acc)) w' in
         List.fold_left seq_merge (List.hd ws) (List.tl ws)
       | w -> w
       end

type (_,_,_,_) lens =
  | Zero  : ('hd0, 'hd1, [`cons of 'hd0 * 'tl] seq, [`cons of 'hd1 * 'tl] seq) lens
  | Succ : ('a, 'b, 'tl0 seq, 'tl1 seq) lens
           -> ('a,'b, [`cons of 'hd * 'tl0] seq, [`cons of 'hd * 'tl1] seq) lens

let rec get : type a b xs ys. (a, b, xs, ys) lens -> xs -> a Mergeable.t = fun ln xs ->
  match ln with
  | Zero -> seq_head xs
  | Succ ln' -> get ln' (seq_tail xs)

let rec put : type a b xs ys. (a,b,xs,ys) lens -> xs -> b Mergeable.t -> ys =
  fun ln xs b ->
  match ln with
  | Zero -> Seq(b, seq_tail xs)
  | Succ ln' -> Seq(seq_head xs, put ln' (seq_tail xs) b)

let goto : type t. t seq lazy_t -> t seq = fun xs ->
  SeqGuard [xs]

(*
 * remove_guards_seq_full:
 * try to expand recursion variables which occurs right under the
 * fixpoint combinator.
 * called during global combinators (-->, choice_at, fix, ..) are called 
 *)
let rec remove_guards_seq_full : type t. t seq -> t seq = function
  | Seq(hd,tl) ->
     let tl =
       try
         remove_guards_seq_full tl
       with
         UnguardedLoopSeq ->
         SeqError
     in
     Seq(hd, tl)
  | SeqGuard [w] ->
     (* recursion variable -- try to expand it *)
     remove_guards_seq_full (remove_guards_seq [] w)
  | SeqGuard ((_::_) as ws) ->
     (* choice involves recursion variable -- do not try to evaluate.
      * Mergeable.force will resolve it later (get_ep)
      *)
     SeqGuard ws
  | SeqAll(_) as xs -> xs
  | SeqGuard [] -> SeqGuard [] (* ??? *)
  | SeqError -> SeqError
  
let fix : type t. (t seq -> t seq) -> t seq = fun f ->
  let rec body =
    lazy begin
        f (SeqGuard [body])
      end
  in
  (* Lazy.force body *)
  match Lazy.force body with
  | SeqGuard [xs] when xs==body -> raise UnguardedLoopSeq
  | xs -> remove_guards_seq_full xs

type ('robj,'c,'a,'b,'xs,'ys) role =
  {label:('robj,'c) method_;
   lens:('a,'b,'xs,'ys) lens}

type close = Close

let get_ep r g =
  let ep = get r.lens g in
  Mergeable.out_ ep

let a = {label={make_obj=(fun v->object method role_A=v end);
                call_obj=(fun o->o#role_A)};
         lens=Zero}
let b = {label={make_obj=(fun v->object method role_B=v end);
                call_obj=(fun o->o#role_B)};
         lens=Succ Zero}
let c = {label={make_obj=(fun v->object method role_C=v end);
                call_obj=(fun o->o#role_C)};
         lens=Succ (Succ Zero)}
let d = {label={make_obj=(fun v->object method role_D=v end);
                call_obj=(fun o->o#role_D)};
         lens=Succ (Succ (Succ Zero))}

module type LIN = sig
  type 'a lin
  val mklin : 'a -> 'a lin
  val unlin : 'a lin -> 'a
end

module type EVENT = sig
  type 'a event
  val guard : (unit -> 'a event) -> 'a event
  val choose : 'a event list -> 'a event
  val wrap : 'a event -> ('a -> 'b) -> 'b event
end
module LwtEvent = struct
  type 'a event = 'a Lwt.t
  let guard f = f () (*XXX correct??*)
  let choose = Lwt.choose
  let wrap e f = Lwt.map f e
end

type ('la,'lb,'va,'vb) label =
  {obj: ('la, 'va) method_;
   var: 'vb -> 'lb}

let msg =
  {obj={make_obj=(fun f -> object method msg=f end);
        call_obj=(fun o -> o#msg)};
   var=(fun v -> `msg(v))}
let left =
  {obj={make_obj=(fun f -> object method left=f end);
        call_obj=(fun o -> o#left)};
   var=(fun v -> `left(v))}
let right =
  {obj={make_obj=(fun f -> object method right=f end);
        call_obj=(fun o -> o#right)};
   var=(fun v -> `right(v))}
let middle =
  {obj={make_obj=(fun f -> object method middle=f end);
        call_obj=(fun o -> o#middle)};
   var=(fun v -> `middle(v))}
  
let left_or_right =
  {obj_merge=(fun l r -> object method left=l#left method right=r#right end);
   obj_splitL=(fun lr -> (lr :> <left : _>));
   obj_splitR=(fun lr -> (lr :> <right : _>));
  }
let right_or_left =
  {obj_merge=(fun l r -> object method right=l#right method left=r#left end);
   obj_splitL=(fun lr -> (lr :> <right : _>));
   obj_splitR=(fun lr -> (lr :> <left : _>));
  }
let to_b m =
  {obj_merge=(fun l r -> object method role_B=m.obj_merge l#role_B r#role_B end);
   obj_splitL=(fun lr -> object method role_B=m.obj_splitL lr#role_B end);
   obj_splitR=(fun lr -> object method role_B=m.obj_splitR lr#role_B end);
  }
let b_or_c =
  {obj_merge=(fun l r -> object method role_B=l#role_B method role_C=r#role_C end);
   obj_splitL=(fun lr -> (lr :> <role_B : _>));
   obj_splitR=(fun lr -> (lr :> <role_C : _>));
  }
