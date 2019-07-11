(**
 * Mergeable: the module for merging endpoints
 *)

open Base
open Common

type 'a t =
  | Single of 'a single
  (** (A) delayed merge involving recvars *)
  | Merge of 'a single list * 'a cache
and 'a single =
  (** fully resolved merge *)
  | Val : 'a body * hook -> 'a single
  (** (B) disjoint merge involving recvars  (output) *)
  | DisjMerge   : 'l t * 'r t * ('lr,'l,'r) disj_merge * 'lr cache -> 'lr single
  (** (C) a recursion variable *) 
  | RecVar : 'a t lazy_t * 'a cache -> 'a single
and 'a body =
  {mergefun: 'a -> 'a -> 'a;
   value: 'a}
and 'a cache = 'a lazy_t
and hook = unit lazy_t

exception UnguardedLoop

let merge_body (ll,hl) (rr,hr) =
  let hook = lazy (Lazy.force hl; Lazy.force hr) in
  ({mergefun=ll.mergefun;
    value=ll.mergefun ll.value rr.value},
   hook)

let disj_merge_body
    : 'lr 'l 'r. ('lr,'l,'r) disj_merge -> 'l body * hook -> 'r body * hook -> 'lr body * hook =
  fun mrg (bl,hl) (br,hr) ->
  let mergefun lr1 lr2 =
    mrg.disj_merge
      (bl.mergefun (mrg.disj_splitL lr1) (mrg.disj_splitL lr2))
      (br.mergefun (mrg.disj_splitR lr1) (mrg.disj_splitR lr2))
  in
  let value = mrg.disj_merge bl.value br.value
  in
  {value; mergefun},lazy (Lazy.force hl; Lazy.force hr)    

(**
 * Resolve delayed merges
 *)
let rec resolve_merge : type x. x t lazy_t list -> x t -> x body * hook = fun hist t ->
  match t with
  | Single s ->
     resolve_merge_single hist s
  | Merge (ss, _) ->
     (* (A) merge involves recursion variables *)
     resolve_merge_list hist ss

and resolve_merge_single : type x. x t lazy_t list -> x single -> x body * hook = fun hist ->
  function
  | Val (v,hook) ->
     (* already resolved *)
     (v,hook)
  | DisjMerge (l,r,mrg,d) ->
     (* (B) disjoint merge involves recursion variables *)
     (* we can safely reset the history; as the split types are different from the merged one, the same type variable will not occur. *)
     let l, hl = resolve_merge [] l in
     let r, hr = resolve_merge [] r in
     disj_merge_body mrg (l,hl) (r,hr)
  | RecVar (t, d) ->
     (* (C) a recursion variable *)
     if find_physeq hist t then begin
         (* we found μt. .. ⊔ t ⊔ .. *)
         raise UnguardedLoop
       end else
       (* force it, and resolve it. at the same time, check that t occurs again or not by adding t to the history  *)
       let b, _ = resolve_merge (t::hist) (Lazy.force t) in
       b, Lazy.from_val () (* dispose the hook -- recvar is already evaluated *)

and resolve_merge_list : type x. x t lazy_t list -> x single list -> x body * hook = fun hist ss ->
  (* remove unguarded recursions *)
  let solved : (x body * hook) list =
    List.fold_left (fun acc u ->
        try
          resolve_merge_single hist u :: acc
        with
          UnguardedLoop ->
          prerr_endline "WARNING: an unbalanced loop detected";
          (* remove it. *)
          acc)
      [] ss
  in
  (* then, merge them altogether *)
  match solved with
  | [] ->
     raise UnguardedLoop
  | x::xs ->
     List.fold_left merge_body x xs

let force_mergeable : 'a. 'a t -> 'a = fun t ->
  let v,hook = resolve_merge [] t in
  Lazy.force hook ;
  v.value
  
let make ~hook ~mergefun ~value =
  Single (Val ({mergefun;value}, hook))

let make_recvar_single t =
  let rec d = RecVar (t, lazy (force_mergeable (Single d)))
  in d

let make_recvar t =
  Single (make_recvar_single t)

let make_merge_single : 'a. 'a single list -> 'a t = fun us ->
  let rec d = Merge (us, lazy (force_mergeable d))
  in d

let make_merge : 'a. 'a t -> 'a t -> 'a t = fun l r ->
  match l, r with
  | Single (Val (ll,hl)), Single (Val (rr,hr)) ->
     let blr, hlr = merge_body (ll,hl) (rr,hr) in
     Single (Val (blr, hlr))
  | Single v1, Single v2 ->
     make_merge_single [v1; v2]
  | Single v, Merge (ds,_) | Merge (ds,_), Single v ->
     make_merge_single (v :: ds)
  | Merge (d1, _), Merge (d2, _) ->
     make_merge_single (d1 @ d2)

let make_merge_list = function
  | [] -> failwith "merge_all: empty"
  | m::ms -> List.fold_left make_merge m ms

let make_disj_merge : 'lr 'l 'r. ('lr,'l,'r) disj_merge -> 'l t -> 'r t -> 'lr t = fun mrg l r ->
  match l, r with
  | Single (Val (bl, hl)), Single (Val (br, hr)) ->
     let blr,hlr = disj_merge_body mrg (bl,hl) (br,hr) in
     Single (Val (blr, hlr))
  | _ ->
     let rec d = Single (DisjMerge (l,r,mrg, lazy (force_mergeable d)))
                        (* prerr_endline "WARNING: internal choice involves recursion variable"; *)
     in d

let mapbody : 'p 'q 'x. ('p -> 'q) -> ('q -> 'p) -> 'p body -> 'q body = fun f g b ->
  {value=f b.value;
   mergefun=(fun l r -> f (b.mergefun (g l) (g r)))}

let rec map_single : 'p 'q 'x. ('p -> 'q) -> ('q -> 'p) -> 'p single -> 'q single = fun f g ->
  function
  | Val (b,h) ->
     Val (mapbody f g b,h)
  | RecVar (t, _) ->
     assert false
  (* make_recvar_single (lazy (map f g (Lazy.force t))) *)
  | DisjMerge (l,r,mrg,d) ->
     assert false

and map : 'p 'q 'x. ('p -> 'q) -> ('q -> 'p) -> 'p t -> 'q t = fun f g ->
  function
  | Single s ->
     Single (map_single f g s)
  | Merge (ss, _) ->
     make_merge_list (List.map (fun s -> Single (map_single f g s)) ss)

let resolve t =
  match t with
  | Single (Val (b,h)) ->
     Lazy.force h;
     b.value
  | Single (RecVar (_,d)) ->
     Lazy.force d
  | Single (DisjMerge (_,_,_,d)) ->
     Lazy.force d
  | Merge (_,d) ->
     Lazy.force d
