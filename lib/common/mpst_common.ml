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
 * It is a function of type `('a option -> 'a)` which returns (1) the merged endpoint when
 * another endpoint `(Some ep)` is passed, or (2) the endpoint itself when the argument is `None`.
 * 
 * Mergeables enable endpoints to be merged based on the object structure.
 * In ocaml-mpst, endpoints are nested objects (and events).
 * The problem is that, in OCaml type system, one can not inspect the object structure 
 * when its type is not known.
 * A mergeble is a bundle of an endpoint and its merging strategy, providing a way 
 * to merge two endpoints of the same type into one.
 *
 * Mergeables themselves can be merged with other mergeables using `Mergeable.merge`.
 *
 * Mergeables can be "delayed" by using `Mergeable.make_recvar`. Delayed mergeables are used
 * to encode recursive endpoints. Merging delayed mergeable will also generate a delayed 
 * mergeable.
 * A delayed mergeable are forced when `resolve_merge` is called.
 *
 * In ocaml-mpst, `resolve_merge` is called during "get_ep" phase to ensure that the all
 * mergings are resolved before actual communication will take place.
 *
 * Mergeable can be created with hooks. 
 *)
module Mergeable :
sig
  (** mergeable endpoint *)
  type 'a t
  type hook = unit lazy_t
  (** makes a mergeable from a merge function and a value *)
  val make : ('a -> 'a -> 'a) -> 'a -> 'a t
  (** makes a delayed mergeable (i.e. recursion variable) *) 
  val make_recvar : 'a t lazy_t -> 'a t
  (** makes a mergeable with a hook. The hook is called once the body is called. *)
  val make_with_hook : hook -> ('a -> 'a -> 'a) -> 'a -> 'a t
  (** makes a constant mergeable which does not merge anything, keeping the constant intact.
   *  This is used for making a closed endpoint. *)
  val make_no_merge : 'a -> 'a t
  (** merges two mergeables *)
  val merge : 'a t -> 'a t -> 'a t
  (** merges list of mergeables into one *)
  val merge_all : 'a t list -> 'a t
  (** extract a value from a mergeable *)
  val out : 'a t -> 'a
  (**
   * resolve_merge: resolve all delayed merges.
   * calls to this function from (-->) combinator is delayed until get_ep phase
   * (i.e. after evaluating global combinators) by using hooks.
   *)
  val resolve_merge : 'a t -> unit

  (** wrap mergeable in an object *)
  val wrap_obj : (< .. > as 'o, 'v) method_ -> 'v t -> 'o t

  (** disjoint merging *)
  val disjoint_merge : ('lr,'l,'r) obj_merge -> 'l t -> 'r t -> 'lr t

  val objfun :
    ('v -> 'v -> 'v) ->
    (< .. > as 'a, 'v) method_ -> ('p -> 'v) -> ('p -> 'a) t
  val apply : ('a -> 'b) t -> 'a -> 'b t
end
  = struct
  (**
   * Mergeable will delay all mergings involving recursion variables inside
   * "cache" type. At the same time, all recursion variables are guarded by
   * one of the following constructors if it is guarded by (-->) or choice_at
   * in global expression.
   * When a cache is forced, it checks recurring occurence of a variable inside
   * merging and remove them. Since all recursion variables are guarded, and are 
   * fully evaluated before actual merging happens, 
   * no "CamlinternalLazy.Undefined" exception will occur during merging.
   *)
  type 'a t =
    | Single of 'a single
    | Merge  of 'a single list * 'a cache
  and 'a single =
    | Val    : 'a body * hook -> 'a single
    | RecVar : 'a t lazy_t * 'a cache -> 'a single
    | Disj   : 'l t * 'r t * ('lr,'l,'r) obj_merge * 'lr cache -> 'lr single
  and 'a body =
    {merge: 'a -> 'a -> 'a;
     value: 'a}
  and 'a cache = 'a body lazy_t
  and hook = unit lazy_t

  let make merge value =
    Single (Val ({merge;value}, Lazy.from_val ()))

  let make_with_hook hook mrg v =
    Single (Val ({merge=mrg;value=v}, hook))
    
  let make_no_merge : 'a. 'a -> 'a t  = fun v ->
    Single (Val ({merge=(fun x _ -> x);value=v}, Lazy.from_val ()))
    
  exception UnguardedLoop

  let disj_merge_body
      : 'lr 'l 'r. ('lr,'l,'r) obj_merge -> 'l body * hook -> 'r body * hook -> 'lr body * hook =
    fun mrg (bl,hl) (br,hr) ->
       let merge lr1 lr2 =
         mrg.obj_merge
           (bl.merge (mrg.obj_splitL lr1) (mrg.obj_splitL lr2))
           (br.merge (mrg.obj_splitR lr1) (mrg.obj_splitR lr2))
       in
       let value = mrg.obj_merge bl.value br.value
       in
       {value; merge},lazy (Lazy.force hl; Lazy.force hr)

  let rec out_ : type x. x t lazy_t list -> x t -> x body * hook =
    fun hist t ->
    match t with
    | Single u -> out_single hist u
    | Merge (ds, _) ->
       (* remove unguarded recursions *)
       let solved : (x body * hook) list =
         List.fold_left (fun acc u ->
             try
               out_single hist u :: acc
             with
               UnguardedLoop ->
               prerr_endline "WARNING: an unbalanced loop detected";
               acc)
           [] ds
       in
       (* then, merge them altogether *)
       match solved with
       | [] ->
          raise UnguardedLoop
       | (v,h)::xs ->
          let hook =
            lazy begin
                Lazy.force h;
                List.iter (fun (_,h) -> Lazy.force h) xs
              end
          in
          {value=List.fold_left v.merge v.value (List.map (fun (x,_)->x.value) xs);
           merge=v.merge}, hook
  and out_single : type x. x t lazy_t list -> x single -> x body * hook =
    fun hist ->
      function
      | Val (v,hook) ->
         (v,hook)
      | RecVar (t, d) ->
         if find_physeq hist t then begin
           raise UnguardedLoop
         end else
           let b, _ = out_ (t::hist) (Lazy.force t) in
           b, Lazy.from_val () (* dispose the hook -- recvar is already evaluated *)
      | Disj (l,r,mrg,d) ->
         let l, hl = out_ [] l in
         let r, hr = out_ [] r in
         disj_merge_body mrg (l,hl) (r,hr)

  let ext_ d =
    let v,h = out_ [] d in
    Lazy.force h;
    v

  let merge_lazy_ : 'a. 'a single list -> 'a t = fun us ->
    let rec d = Merge (us, lazy (ext_ d))
    in d

  let merge_disj_ : 'lr 'l 'r. ('lr,'l,'r) obj_merge -> 'l t -> 'r t -> 'lr t =
    fun mrg l r ->
    let rec d = Single (Disj (l,r,mrg, lazy (ext_ d)))
    in d

  let make_recvar_single t =
    let rec d = RecVar (t, lazy (ext_ (Single d)))
    in d

  let make_recvar t =
    Single (make_recvar_single t)

  let merge : 'a. 'a t -> 'a t -> 'a t =
    fun l r ->
    match l, r with
    | Single (Val (ll,hl)), Single (Val (rr,hr)) ->
       let hook = lazy (Lazy.force hl; Lazy.force hr) in
       Single (Val ({merge=ll.merge;
                     value=ll.merge ll.value rr.value},
                    hook))
    | Single v1, Single v2 ->
       merge_lazy_ [v1; v2]
    | Single v, Merge (ds,_) | Merge (ds,_), Single v ->
       merge_lazy_ (v :: ds)
    | Merge (d1, _), Merge (d2, _) ->
       merge_lazy_ (d1 @ d2)

  let merge_all = function
    | [] -> failwith "merge_all: empty"
    | m::ms -> List.fold_left merge m ms

  let out : 'a. 'a t -> 'a = fun t ->
    match t with
    | Single (Val (b,h)) ->
       Lazy.force h;
       b.value
    | Single (RecVar (_,d)) ->
       (Lazy.force d).value
    | Single (Disj (_,_,_,d)) ->
       (Lazy.force d).value
    | Merge (_,d) ->
       (Lazy.force d).value

  let resolve_merge : 'a. 'a t -> unit = fun t ->
    ignore (out t)

  let rec obj_body : 'v. (< .. > as 'o, 'v) method_ -> 'v body -> 'o body = fun meth v ->
    {value=meth.make_obj v.value;
     merge=(fun l r ->
       let ll = meth.call_obj l
       and rr = meth.call_obj r
       in
       meth.make_obj (v.merge ll rr))}

  let rec wrap_obj : 'v. (< .. > as 'o, 'v) method_ -> 'v t -> 'o t = fun meth v ->
    match v with
    | Single u -> Single (wrap_obj_single meth u)
    | Merge (ds,_) ->
       merge_lazy_ (List.map (wrap_obj_single meth) ds)
      
  and wrap_obj_single : 'v. (< .. > as 'o, 'v) method_ -> 'v single -> 'o single = fun meth v ->
    match v with
    | Val (b,h) ->
       Val (obj_body meth b,h)
    | Disj (_,_,_,_) ->
       failwith "wrap_obj_singl: Disj" (* XXX *)
    | RecVar (t, _) ->
       make_recvar_single (lazy (wrap_obj meth (Lazy.force t)))

  let disjoint_merge : 'lr 'l 'r. ('lr,'l,'r) obj_merge -> 'l t -> 'r t -> 'lr t = fun mrg l r ->
    match l, r with
    | Single (Val (bl, hl)), Single (Val (br, hr)) ->
       let blr,hlr = disj_merge_body mrg (bl,hl) (br,hr) in
       Single (Val (blr, hlr))
    | _ ->
       (* prerr_endline "WARNING: internal choice involves recursion variable"; *)
       merge_disj_ mrg l r

  let rec apply_single : 'a 'b. ('a -> 'b) single -> 'a -> 'b single = fun f v ->
    match f with
    | RecVar (t, d) ->
       make_recvar_single (lazy (apply (Lazy.force t) v))
    | Val (b,h) ->
       Val ({value=b.value v; merge=(fun l r -> b.merge (fun _ -> l) (fun _ -> r) v)}, h)
    | Disj (_,_,_,_) ->
       failwith "apply_single: Disj" (* XXX *)
      
  and apply : 'a 'b. ('a -> 'b) t -> 'a -> 'b t = fun f v ->
    match f with
    | Merge(ds, _) ->
       merge_lazy_ (List.map (fun d -> apply_single d v) ds)
    | Single f -> Single (apply_single f v)

  let objfun
      : 'o 'v 'p. ('v -> 'v -> 'v) -> (< .. > as 'o, 'v) method_ -> ('p -> 'v) -> ('p -> 'o) t =
    fun merge meth f  ->
    Single (Val ({value=(fun x -> meth.make_obj (f x));
                  merge=(fun l r x ->
                    meth.make_obj (merge (meth.call_obj (l x)) ((meth.call_obj (r x)))))},
                 Lazy.from_val ()))
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
    | SeqCons(_,_), _ ->
       let hd = Mergeable.merge (seq_head l) (seq_head r) in
       let tl = seq_merge (seq_tail l) (seq_tail r) in
       SeqCons(hd, tl)
    | _, SeqCons(_,_) -> seq_merge r l
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
        * (will happen at after c of fix (fun t -> choice (a->b)t (a->b)t))
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
   * Seq.partial_force tries to fully evaluate unguarded recursion variables 
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
