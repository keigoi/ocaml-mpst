
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

(**
 * A mergeable is a session endpoint which can be merged with another endpoint in future.
 * It is a function of type ('a option -> 'a) which returns (1) the merged endpoint when
 * another endpoint (Some ep) is passed, or (2) the endpoint itself when the argument is None.
 * 
 * Mergeables enable endpoints to be merged based on the object structure.
 * In ocaml-mpst, endpoints are nested objects (and events).
 * The problem is that, in OCaml type system, one can not inspect the object structure 
 * when its type is not known.
 * A mergeble is a bundle of an endpoint and its merging strategy, providing a way 
 * to merge two endpoints of the same type into one.
 *
 * Mergeables themselves can be merged with other mergeables.
 * Specifically, `Mergeable.merge_delayed` will return a "delayed" mergeable which is 
 * used to merge recursive endpoints.
 * A delayed mergeable are forced when `resolve_merge` is called.
 *
 * In ocaml-mpst, `resolve_merge` is called during "get_ep" phase to ensure that the all
 * mergings are resolved before actual communication will take place.
 *)
module Mergeable :
sig
  (** mergeable endpoint *)
  type 'a t
  exception UnguardedLoop
  (** makes a mergeable from a merge function and a value *)
  val make : ('a -> 'a -> 'a) -> 'a -> 'a t
  val make_recvar : 'a t lazy_t -> 'a t
  val make_bare : ('a option -> 'a) -> 'a t
  val make_with_hook : ('a -> unit) -> ('a -> 'a -> 'a) -> 'a -> 'a t
  val make_no_merge : 'a -> 'a t
  val merge : 'a t -> 'a t -> 'a t
  val merge_all : 'a t list -> 'a t
  (** extract a value from a mergeable *)
  val out : 'a t -> 'a
  (** unwrap *)
  val unwrap : 'a t -> 'a option -> 'a
  (**
   * resolve_merge: try to resolve choices which involve recursion variables.
   * calls to this function from (-->) combinator is delayed until get_ep phase
   * (i.e. after evaluating global combinators)
   *)
  val resolve_merge : 'a t -> unit

  (** wrap mergeable in an object *)
  val obj : (< .. > as 'o, 'v) method_ -> 'v t -> 'o t

  val objfun :
    ('v -> 'v -> 'v) ->
    (< .. > as 'a, 'v) method_ -> ('p -> 'v) -> ('p -> 'a) t
  val apply : ('a -> 'b) t -> 'a -> 'b t
end
  = struct
  type 'a t =
    | M of 'a u
    | MDelayMerge of 'a u list
  and 'a u =
    | Val of ('a option -> 'a)
    | RecVar of 'a t lazy_t

  exception UnguardedLoop

  let make mrg v =
    M (Val (function
        | None -> v
        | Some v2 -> mrg v v2))

  let make_with_hook hook mrg v1 =
    M (Val (function
        | None -> (hook v1 : unit); v1
        | Some v2 -> let v12 = mrg v1 v2 in hook v12; v12))
    
  let make_bare : 'a. ('a option -> 'a) -> 'a t  = fun v ->
    M (Val v)
             
  let make_no_merge : 'a. 'a -> 'a t  = fun v ->
    M (Val (fun _ -> v))

  let make_recvar d = M (RecVar d)
    
  type 'a merge__ = 'a option -> 'a

  let merge0 : type x. x merge__ -> x merge__ -> x merge__ = fun l r ->
    fun obj -> l (Some (r obj))

  let merge : 'a. 'a t -> 'a t -> 'a t =
    fun l r ->
    match l, r with
    | M (Val ll), M (Val rr) ->
       M (Val (merge0 ll rr))
    | M v1, M v2 ->
       MDelayMerge [v1; v2]
    | M v, MDelayMerge ds | MDelayMerge ds, M v ->
       MDelayMerge (v :: ds)
    | MDelayMerge d1, MDelayMerge d2 ->
       MDelayMerge (d1 @ d2)

  let merge_all = function
    | [] -> failwith "merge_all: empty"
    | m::ms -> List.fold_left merge m ms

  let rec out_checked : type x. x t lazy_t list -> x t -> x option -> x =
    fun hist t ->
    let resolve d =
      if find_physeq hist d then
        raise UnguardedLoop
      else
        out_checked (d::hist) (Lazy.force d)
    in
    match t with
    | M (Val x) -> x
    | M (RecVar d) ->
       resolve d
    | MDelayMerge ds ->
       let solved =
         List.fold_left (fun acc d ->
             match d with
             | Val v -> v :: acc
             | RecVar d ->
                try
                  resolve d :: acc
                with
                  UnguardedLoop -> acc)
           [] ds
       in
       match solved with
       | [] ->
          raise UnguardedLoop
       | x::xs ->
          List.fold_left merge0 x xs

  let out : 'a. 'a t -> 'a = fun g ->
    out_checked [] g None

  let unwrap : 'a. 'a t -> 'a option -> 'a = fun g ->
    out_checked [] g

  let resolve_merge : 'a. 'a t -> unit = fun t ->
    match t with
    | M (Val _) ->
       () (* already evaluated *)
    | M (RecVar _) ->
       () (* do not touch -- this is a recursion variable *)
    | MDelayMerge _ ->
       (* try to resolve it -- a choice involving recursion variable(s) *)
       let _ = out_checked [] t in ()

  let rec applybody : 'a 'b. ('a -> 'b) u -> 'a -> 'b u = fun f v ->
    match f with
    | RecVar d ->
       RecVar (lazy (apply (Lazy.force d) v))
    | Val f ->
       Val (fun othr ->
           match othr with
           | Some othr -> f (Some (fun _ -> othr)) v (* XXX *)
           | None -> f None v)
      
  and apply : 'a 'b. ('a -> 'b) t -> 'a -> 'b t = fun f v ->
    match f with
    | MDelayMerge(ds) ->
       MDelayMerge(List.map (fun d -> applybody d v) ds)
    | M f -> M (applybody f v)

  let rec obj_raw = fun meth f ->
    function
    | None ->
       meth.make_obj (f None)
    | Some o ->
       meth.make_obj (f (Some (meth.call_obj o)))
                           
  let rec obj : 'v. (< .. > as 'o, 'v) method_ -> 'v t -> 'o t = fun meth v ->
    match v with
    | M (Val f) ->
       M (Val (obj_raw meth f))
    | M (RecVar d) ->
       M (RecVar (lazy (obj meth (Lazy.force d))))
    | MDelayMerge ds ->
       MDelayMerge (List.map
                     (function
                      | Val f -> Val (obj_raw meth f)
                      | RecVar d -> RecVar (lazy (obj meth (Lazy.force d)))) ds)

  let objfun
      : 'o 'v 'p. ('v -> 'v -> 'v) -> (< .. > as 'o, 'v) method_ -> ('p -> 'v) -> ('p -> 'o) t =
    fun merge meth val_ ->
    make_bare (fun obj p ->
        match obj with
        | None ->
           meth.make_obj (val_ p)
        | Some obj ->
           let val2 = meth.call_obj (obj p) in
           meth.make_obj (merge (val_ p) val2))
end

(**
 * The module for sequences of mergeables (endpoints).
 *)
module Seq :
sig
  (** sequence type *)
  type _ t = (* can we hide these constructors? *)
    (** cons *)
    | SeqCons : 'hd Mergeable.t * 'tl t -> [ `cons of 'hd * 'tl ] t
    (** repetition -- for closed endpoints *)
    | SeqRepeat : 'a Mergeable.t -> ([ `cons of 'a * 'b ] as 'b) t
    (** recursion variable(s) *)
    | SeqRecVars : 'a seqvar list -> 'a t
    (** unguarded loop; we have it in the last part of a recursion *)
    | SeqBottom : 'x t
  and 'a seqvar = 'a t lazy_t

  (** lenses in constructor form *)
  type (_, _, _, _) lens =
      Zero : ('hd0, 'hd1, [ `cons of 'hd0 * 'tl ] t, [ `cons of 'hd1 * 'tl ] t) lens
    | Succ :
        ('a, 'b, 'tl0 t, 'tl1 t) lens
        -> ('a, 'b, [ `cons of 'hd * 'tl0 ] t, [ `cons of 'hd * 'tl1 ] t) lens

  (** raised when one would try to extract a value from unguarded loop *)
  exception UnguardedLoopSeq

  (** lens get function *)
  val get : ('a, 'b, 'xs, 'ys) lens -> 'xs -> 'a Mergeable.t
  (** lens put function *)
  val put : ('a, 'b, 'xs, 'ys) lens -> 'xs -> 'b Mergeable.t -> 'ys

  (** merging of two sequences for choice  *)
  val seq_merge : 'x t -> 'x t -> 'x t

  (**
   * partial_force:
   * it tries to expand unguarded recursion variables which occurs right under the
   * fixpoint combinator. This enables a "fail-fast" policy to handle unguarded recursions --
   * it would raise an exception if there is an unguarded occurrence of a recursion variable.
   * This fuction is called during the initial construction phase of an 
   * endpoint sequence.
   *)
  val partial_force : 'x t lazy_t list -> 'x t -> 'x t
end = struct
  type _ t =
    | SeqCons : 'hd Mergeable.t * 'tl t -> [`cons of 'hd * 'tl] t
    | SeqRepeat : 'a Mergeable.t -> ([`cons of 'a * 'tl] as 'tl) t
    | SeqRecVars : 'a seqvar list -> 'a t
    | SeqBottom : 'x t
  and 'a seqvar = 'a t lazy_t

  type (_,_,_,_) lens =
    | Zero  : ('hd0, 'hd1, [`cons of 'hd0 * 'tl] t, [`cons of 'hd1 * 'tl] t) lens
    | Succ : ('a, 'b, 'tl0 t, 'tl1 t) lens
             -> ('a,'b, [`cons of 'hd * 'tl0] t, [`cons of 'hd * 'tl1] t) lens

  exception UnguardedLoopSeq
          
  let rec seq_head : type hd tl. [`cons of hd * tl] t -> hd Mergeable.t =
    function
    | SeqCons(hd,_) -> hd
    | SeqRecVars ds -> Mergeable.merge_all (List.map seqvar_head ds)
    | SeqRepeat(a) -> a
    | SeqBottom -> raise UnguardedLoopSeq
  and seqvar_head : type hd tl. [`cons of hd * tl] t lazy_t -> hd Mergeable.t = fun d ->
    Mergeable.make_recvar (lazy (seq_head (Lazy.force d)))

  let rec seq_tail : type hd tl. [`cons of hd * tl] t -> tl t =
    function
    | SeqCons(_,tl) -> tl
    | SeqRecVars ds -> SeqRecVars(List.map seqvar_tail ds)
    | (SeqRepeat _) as s -> s
    | SeqBottom -> raise UnguardedLoopSeq
  and seqvar_tail : type hd tl. [`cons of hd * tl] t lazy_t -> tl t lazy_t = fun d ->
    lazy (seq_tail (Lazy.force d))

  let rec get : type a b xs ys. (a, b, xs, ys) lens -> xs -> a Mergeable.t = fun ln xs ->
    match ln with
    | Zero -> seq_head xs
    | Succ ln' -> get ln' (seq_tail xs)

  let rec put : type a b xs ys. (a,b,xs,ys) lens -> xs -> b Mergeable.t -> ys =
    fun ln xs b ->
    match ln with
    | Zero -> SeqCons(b, seq_tail xs)
    | Succ ln' -> SeqCons(seq_head xs, put ln' (seq_tail xs) b)

  let rec seq_merge : type x. x t -> x t -> x t = fun l r ->
    match l,r with
    | _, SeqCons(_,_) -> seq_merge r l
    | SeqCons(_,_), _ ->
       let hd = Mergeable.merge (seq_head l) (seq_head r) in
       let tl = seq_merge (seq_tail l) (seq_tail r) in
       SeqCons(hd, tl)
    (* delayed constructors are left as-is *)
    | SeqRecVars(us1), SeqRecVars(us2) -> SeqRecVars(us1 @ us2)
    (* repeat *)
    | SeqRepeat(_), _ -> l
    | _, SeqRepeat(_) -> r
    (* bottom *)
    | SeqBottom,_  -> raise UnguardedLoopSeq
    | _, SeqBottom -> raise UnguardedLoopSeq

  let rec resolve_seqvar_ : type x. x t lazy_t list -> x t lazy_t -> x t =
    fun hist w ->
    if find_physeq hist w then begin
        raise UnguardedLoopSeq
      end else begin
        match Lazy.force w with
        | SeqRecVars [w'] -> resolve_seqvar_ (w::hist) w'
        | s -> s
      end

  let rec partial_force : type x. x t lazy_t list -> x t -> x t =
    fun hist ->
    function
    | SeqRecVars [d] ->
       (* recursion variable -- try to expand it *)
       partial_force [] (resolve_seqvar_ [] d)
    | SeqRecVars ds ->
       (* A choice between recursion variables -- do not try to resolve. 
        * (FIXME: when does this happen actually)
        * Mergeable.resolve_merge will resolve it later during get_ep
        *)
       SeqRecVars ds
    | SeqCons(hd,tl) ->
       let tl =
         try
           partial_force [] tl
         with
           UnguardedLoopSeq ->
           (* we do not raise exception here; 
            * in recursion, an unguarded loop will occur in the last part of the sequence.
            * when one tries to take head/tail of SeqBottom, an exception will be raised.
            *)
           SeqBottom
       in
       SeqCons(hd, tl)
    | SeqRepeat(_) as xs -> xs
    | SeqBottom -> SeqBottom
end

let fix : type t. (t Seq.t -> t Seq.t) -> t Seq.t = fun f ->
  let rec body =
    lazy begin
        f (SeqRecVars [body])
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

let get_ep : ('x0, 'x1, 'ep, 'x2, 'seq, 'x3) role -> 'seq -> 'ep = fun r g ->
  let ep = Seq.get r.lens g in
  Mergeable.out ep

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
