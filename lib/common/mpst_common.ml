
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


(*
 * A mergeable is a session endpoint which can be merged with another endpoint in future.
 * It is a function of type ('a option -> 'a) which returns (1) the merged endpoint when
 * another endpoint (Some ep) is applied, or (2) the endpoint itself when None is passed.
 * 
 * Mergeables enable to merge endpoints based on the object structure.
 * In ocaml-mpst, endpoints are nested sequence of objects (and events).
 * The problem is that OCaml type system does not allow to inspect the object structure 
 * when its type is not known. 
 * Mergebles resolve this by combining an endpoint and its merging strategy into one 
 * datatype (Mergeable.t).
 *
 * Mergeables themselves can be merged with other mergeables.
 * Specifically, `Mergeable.merge_delayed` will return a "delayed" mergeable which is 
 * used to merge recursive endpoints.
 * A delayed mergeable are forced when `resolve_merge` is called.
 *
 * In ocaml-mpst, `resolve_merge` is called during "get_ep" phase to ensure that the all
 * mergings are resolved before actual communication will take place.
 *)
module Mergeable
: sig
  type 'a t = Val of ('a option -> 'a) | Delayed of 'a t lazy_t list
  exception UnguardedLoop
  val make : ('v -> 'v -> 'v) -> 'v -> 'v t
  val make_with_hook : ('v -> unit) -> ('v -> 'v -> 'v) -> 'v -> 'v t
  val bare_ : ('a option -> 'a) -> 'a t
  val no_merge : 'a -> 'a t
  val merge : 'a t -> 'a t -> 'a t
  val merge_delayed : 'a t lazy_t list -> 'a t
  val out_ : 'a t -> 'a
  val out : 'a t -> 'a option -> 'a
  val resolve_merge : 'a t -> unit
  val apply : ('a -> 'b) t -> 'a -> 'b t
  val obj :
    (< .. > as 'o, 'v) method_ -> 'v t -> 'o t
  val objfun :
    ('v -> 'v -> 'v) ->
    (< .. > as 'o, 'v) method_ -> ('p -> 'v) -> ('p -> 'o) t
  (* val hook : ('v t -> unit) -> 'v t -> 'v t *)
end
  = struct
  type 'a t =
    | Val of ('a option -> 'a)
    | Delayed of 'a t lazy_t list (* delayed merging *)

  exception UnguardedLoop

  let make mrg v =
    Val (function
        | None -> v
        | Some v2 -> mrg v v2)

  let make_with_hook hook mrg v1 =
    Val (function
        | None -> (hook v1 : unit); v1
        | Some v2 -> let v12 = mrg v1 v2 in hook v12; v12)

  type 'a merge__ = 'a option -> 'a

  let merge0 : type x. x merge__ -> x merge__ -> x merge__ = fun l r ->
    fun obj -> l (Some (r obj))

  let merge : 'a. 'a t -> 'a t -> 'a t =
    fun l r ->
    match l, r with
    | Val ll, Val rr ->
       Val (merge0 ll rr)
    | Delayed d1, Delayed d2 ->
       Delayed (d1 @ d2)
    | ((Val _) as v, Delayed d) | (Delayed d, ((Val _) as v)) ->
       Delayed ([lazy v] @ d)

  let merge_delayed : 'a. 'a t lazy_t list -> 'a t  = fun v ->
    Delayed v
    
  let no_merge : 'a. 'a -> 'a t  = fun v ->
    Val (fun _ -> v)
    
  let bare_ : 'a. ('a option -> 'a) -> 'a t  = fun v ->
    Val v

  let rec out__ : type x. x t lazy_t list -> x t -> x option -> x =
    fun hist t ->
    match t with
    | (Val x) -> x
    | Delayed ds ->
       let solved =
         List.fold_left (fun acc d ->
             try
               if find_physeq hist d then begin
                   acc (* found a loop. drop it *)
                 end else begin
                   let g = out__ (d::hist) (Lazy.force d)
                   in g :: acc
                 end
             with
               UnguardedLoop -> acc)
           [] ds
       in
       match solved with
       | [] ->
          raise UnguardedLoop
       | x::xs ->
          List.fold_left merge0 x xs

  let out_ : 'a. 'a t -> 'a = fun g ->
    out__ [] g None

  let out : 'a. 'a t -> 'a option -> 'a = fun g ->
    out__ [] g

  (* 
   * resolve_merge: try to resolve choices which involve recursion variables.
   * calls to this function from (-->) combinator is delayed until get_ep phase
   * (i.e. after evaluating global combinators)
   *)
  let resolve_merge : 'a. 'a t -> unit = fun t ->
    match t with
    | (Val _) ->
       () (* already evaluated *)
    | (Delayed []) ->
       () (* ??? *)
    | (Delayed [_]) ->
       () (* do not touch -- this is a recursion variable *)
    | (Delayed (_::_::_)) ->
       (* try to resolve it -- a choice involving recursion variable(s) *)
       let _ = out__ [] t in ()

  let rec apply : 'a 'b. ('a -> 'b) t -> 'a -> 'b t = fun f v ->
    match f with
    | Delayed(ds) ->
       Delayed(List.map (fun d -> lazy (apply (Lazy.force d) v)) ds)
    | Val f ->
       Val (fun othr ->
           match othr with
           | Some othr -> f (Some (fun _ -> othr)) v (* XXX *)
           | None -> f None v)

  let rec obj : 'v. (< .. > as 'o, 'v) method_ -> 'v t -> 'o t = fun meth v ->
    match v with
    | Val f ->
       Val (function
           | None ->
              meth.make_obj (f None)
           | Some o ->
              meth.make_obj (f (Some (meth.call_obj o))))
    | Delayed ds ->
        Delayed (List.map (fun d -> lazy (obj meth (Lazy.force d))) ds)

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

(*
 * The module for the sequence of endpoints.
 * 
 *)
module Seq = struct
  type _ t =
    | Seq : 'hd Mergeable.t * 'tl t -> [`cons of 'hd * 'tl] t
    | SeqDelayed : 'x t lazy_t list -> 'x t
    | SeqRepeat : 'a Mergeable.t -> ([`cons of 'a * 'tl] as 'tl) t
    | SeqBottom : 'x t

  type (_,_,_,_) lens =
    | Zero  : ('hd0, 'hd1, [`cons of 'hd0 * 'tl] t, [`cons of 'hd1 * 'tl] t) lens
    | Succ : ('a, 'b, 'tl0 t, 'tl1 t) lens
             -> ('a,'b, [`cons of 'hd * 'tl0] t, [`cons of 'hd * 'tl1] t) lens

  exception UnguardedLoopSeq

  let rec seq_head : type hd tl. [`cons of hd * tl] t -> hd Mergeable.t =
    function
    | Seq(hd,_) -> hd
    | SeqRepeat(a) -> a
    | SeqDelayed xss ->
       let xss = List.map (fun xs -> lazy (seq_head (Lazy.force xs))) xss
       in
       Mergeable.merge_delayed xss
    | SeqBottom -> raise UnguardedLoopSeq

  let rec seq_tail : type hd tl. [`cons of hd * tl] t -> tl t = fun xs ->
    match xs with
    | Seq(_,tl) -> tl
    | (SeqRepeat _) as s -> s
    | SeqDelayed xss -> SeqDelayed(List.map (fun xs -> lazy (seq_tail (Lazy.force xs))) xss)
    | SeqBottom -> raise UnguardedLoopSeq

  let rec get : type a b xs ys. (a, b, xs, ys) lens -> xs -> a Mergeable.t = fun ln xs ->
    match ln with
    | Zero -> seq_head xs
    | Succ ln' -> get ln' (seq_tail xs)

  let rec put : type a b xs ys. (a,b,xs,ys) lens -> xs -> b Mergeable.t -> ys =
    fun ln xs b ->
    match ln with
    | Zero -> Seq(b, seq_tail xs)
    | Succ ln' -> Seq(seq_head xs, put ln' (seq_tail xs) b)

  let rec seq_merge : type x. x t -> x t -> x t = fun l r ->
    match l,r with
    | Seq(hd_l,tl_l), Seq(hd_r, tl_r) ->
       Seq(Mergeable.merge hd_l hd_r, seq_merge tl_l tl_r)
    | SeqDelayed(xss), Seq(hd_r, tl_r) ->
       let hd_l =
         Mergeable.merge_delayed (List.map (fun xs -> lazy (seq_head (Lazy.force xs))) xss)
       in
       let tl_l = SeqDelayed(List.map (fun xs -> lazy (seq_tail (Lazy.force xs))) xss) in
       Seq(Mergeable.merge hd_l hd_r, seq_merge tl_l tl_r)
    | (Seq(_, _) as l), (SeqDelayed(_) as r) ->
       seq_merge r l
    | _, SeqRepeat(a) -> SeqRepeat(a)
    | SeqRepeat(a), _ -> SeqRepeat(a)
    | SeqDelayed(w1), SeqDelayed(w2) -> SeqDelayed(w1 @ w2)
    | SeqBottom,_  -> raise UnguardedLoopSeq
    | _, SeqBottom -> raise UnguardedLoopSeq

  let rec resolve_delayed_ : type x. x t lazy_t list -> x t lazy_t -> x t =
    fun hist w ->
    if find_physeq hist w then begin
        raise UnguardedLoopSeq
      end else begin
        match Lazy.force w with
        | SeqDelayed w' ->
           let ws = List.map (resolve_delayed_ (w::hist)) w' in
           List.fold_left seq_merge (List.hd ws) (List.tl ws)
        | w -> w
      end

  (*
   * partial_force:
   * it tries to expand unguarded recursion variables which occurs right under the
   * fixpoint combinator. This enables a "fail-fast" policy to handle unguarded recursions --
   * it would raise an exception if there is an unguarded occurrence of a recursion variable.
   * This fuction is called during the initial construction phase of an 
   * endpoint sequence.
   *)
  let rec partial_force : type x. x t lazy_t list -> x t -> x t =
    fun hist ->
    function
    | SeqDelayed [w] ->
       (* recursion variable -- try to expand it *)
       partial_force [] (resolve_delayed_ [] w)
    | SeqDelayed ((_::_::_) as xs) ->
       (* A choice with recursion variables -- do not try to resolve.
        * Mergeable.resolve_merge will resolve it later during get_ep
        *)
       SeqDelayed xs
    | SeqDelayed [] -> SeqDelayed [] (* ??? *)
    | Seq(hd,tl) ->
       let tl =
         try
           partial_force [] tl (* FIXME use (map seq_tail hist) ? *)
         with
           UnguardedLoopSeq -> SeqBottom
       in
       Seq(hd, tl)
    | SeqRepeat(_) as xs -> xs
    | SeqBottom -> SeqBottom
end
  
let fix : type t. (t Seq.t -> t Seq.t) -> t Seq.t = fun f ->
  let rec body =
    lazy begin
        f (SeqDelayed [body])
      end
  in
  (* A "fail-fast" approach to detect unguarded loops.
   * Seq.partial_force tres to fully evaluate unguarded recursion variables 
   * in the body.
   *)
  Seq.partial_force [body] (Lazy.force body)

type ('robj,'c,'a,'b,'xs,'ys) role =
  {label:('robj,'c) method_;
   lens:('a,'b,'xs,'ys) Seq.lens}

type close = Close

let get_ep r g =
  let ep = Seq.get r.lens g in
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
