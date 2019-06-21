(**
 * Mergeable: the module for merging endpoints
 * see MERGEABLE in s.ml 
 *)

open Base
open Common

(**
 * The type of a mergeable.
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
   value: 'a ep list}
and 'a cache = 'a body lazy_t
and hook = unit lazy_t

let make merge value =
  Single (Val ({merge;value}, Lazy.from_val ()))

let make_with_hook hook mrg v =
  Single (Val ({merge=mrg;value=v}, hook))
  
let make_no_merge : 'a. 'a list -> 'a t  = fun vs ->
  Single (Val ({merge=(fun x _ -> x);
                value=List.map (fun v _ -> v) vs},
               Lazy.from_val ()))
  
exception UnguardedLoop

let disj_merge_body
    : 'lr 'l 'r. ('lr,'l,'r) obj_merge -> 'l body * hook -> 'r body * hook -> 'lr body * hook =
  fun mrg (bl,hl) (br,hr) ->
  let merge lr1 lr2 =
    mrg.obj_merge
      (bl.merge (mrg.obj_splitL lr1) (mrg.obj_splitL lr2))
      (br.merge (mrg.obj_splitR lr1) (mrg.obj_splitR lr2))
  in
  let value =
    (* we can only choose one of them -- distribute the linearity flag among merged objects *)
    List.map2 (fun l r -> fun f -> mrg.obj_merge (l f) (r f))  bl.value br.value
  in
  {value; merge},lazy (Lazy.force hl; Lazy.force hr)

let mapmerge mrg l r =
  List.map2 (fun l r f -> mrg (l f) (r @@ LinFlag.create ())) l r

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
     | (b,h)::xs ->
        let hook =
          lazy begin
              Lazy.force h;
              List.iter (fun (_,h) -> Lazy.force h) xs
            end
        in
        let value =
          List.fold_left
            (mapmerge b.merge)
            b.value
            (List.map (fun (b1,_)-> b1.value) xs)
        in
        {value; merge=b.merge}, hook
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
                   value=mapmerge ll.merge ll.value rr.value},
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

let out : 'a. 'a t -> 'a list = fun t ->
  let lin fs = List.map (fun f -> f @@ LinFlag.create ()) fs in
  match t with
  | Single (Val (b,h)) ->
     Lazy.force h;
     lin b.value
  | Single (RecVar (_,d)) ->
     lin (Lazy.force d).value
  | Single (Disj (_,_,_,d)) ->
     lin (Lazy.force d).value
  | Merge (_,d) ->
     lin (Lazy.force d).value

let wrap_obj_body : 'v. (< .. > as 'o, 'v) method_ -> 'v body -> 'o body = fun meth b ->
  {value=List.map (fun v f -> meth.make_obj (v f)) b.value;
   merge=(fun l r ->
     let ll = meth.call_obj l
     and rr = meth.call_obj r
     in
     meth.make_obj (b.merge ll rr))}

let rec wrap_obj : 'v. (< .. > as 'o, 'v) method_ -> 'v t -> 'o t = fun meth v ->
  match v with
  | Single u -> Single (wrap_obj_single meth u)
  | Merge (ds,_) ->
     merge_lazy_ (List.map (wrap_obj_single meth) ds)
    
and wrap_obj_single : 'v. (< .. > as 'o, 'v) method_ -> 'v single -> 'o single = fun meth v ->
  match v with
  | Val (b,h) ->
     Val (wrap_obj_body meth b,h)
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
