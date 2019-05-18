
let map_option f = function
  | Some x -> Some (f x)
  | None -> None

type ('la,'va) method_ =
  {make_obj: 'va -> 'la;
   call_obj: 'la -> 'va}

type ('la,'lb,'va,'vb) label =
  {obj: ('la, 'va) method_;
   var: 'vb -> 'lb}

type ('lr, 'l, 'r) obj_merge =
  {obj_merge: 'l -> 'r -> 'lr;
   obj_splitL: 'lr -> 'l;
   obj_splitR: 'lr -> 'r;
  }

type 'a mergeable = 'a option -> 'a

let make_mergeable
    : 'o 'v. ('v -> 'v -> 'v) -> (< .. > as 'o, 'v) method_ -> 'v -> 'o mergeable =
  fun merge meth val_ ->
  fun obj ->
  match obj with
  | None ->
     meth.make_obj val_
  | Some obj ->
     let val2 = meth.call_obj obj in
     meth.make_obj (merge val_ val2)

let make_mergeable_fun
    : 'o 'v 'p. ('v -> 'v -> 'v) -> (< .. > as 'o, 'v) method_ -> ('p -> 'v) -> ('p -> 'o) mergeable =
  fun merge meth val_ ->
  fun obj p ->
  match obj with
  | None ->
     meth.make_obj (val_ p)
  | Some obj ->
     let val2 = meth.call_obj (obj p) in
     meth.make_obj (merge (val_ p) val2)

module Guarded = struct           
  type 'a guarded =
    | Guarded of 'a guarded lazy_t list
    | Val of 'a

  let val_ x = Val x

  exception UnguardedLoop

  let rec find_physeq : 'a. 'a list -> 'a -> bool = fun xs y ->
    match xs with
    | x::xs -> if x==y then true else find_physeq xs y
    | [] -> false

  let rec remove_guards_one
          : type t. (t -> t -> t) -> t guarded lazy_t list -> t guarded -> t
    = fun mergefun hist g ->
    match g with
    | Val x -> x
    | Guarded gs ->
       let solved =
         List.fold_left (fun acc g' ->
             try
               if find_physeq hist g' then begin
                   acc (* found a loop. drop it *)
                 end else
                 remove_guards_one mergefun (g'::hist) (Lazy.force g') :: acc
             with
               UnguardedLoop -> acc) [] gs
       in
       match solved with
       | [] ->
          raise UnguardedLoop
       | x::xs ->
          List.fold_left mergefun x xs

  let guarded_merge : type t. (t -> t -> t) -> t guarded -> t guarded -> t guarded =
    fun mergefun l r ->
    match l, r with
    | Val ll, Val rr ->
       Val (mergefun ll rr)
    | Guarded g1, Guarded g2 ->
       Guarded (g1 @ g2)
    | ((Val _) as b, Guarded g) | (Guarded g, ((Val _) as b)) ->
       Guarded ([lazy b] @ g)

end

module type PARAM = sig
  type t
end

open Guarded

type 'p prot_ = 'p option -> 'p

let prot_merge_ : type t. t prot_ -> t prot_ -> t prot_ = fun l r ->
  fun obj -> l (Some (r obj))

type 'a prot = 'a prot_ guarded

let prot_merge l r = guarded_merge prot_merge_ l r

let rec prot_apply : 'a 'b. ('a -> 'b) prot -> 'a -> 'b prot = fun g v ->
  match g with
  | Guarded(gs) ->
     Guarded(List.map (fun g -> lazy (prot_apply (Lazy.force g) v)) gs)
  | Val f ->
     Val (fun w ->
         match w with
         | Some w -> f (Some (fun _ -> w)) v
         | None -> f None v)

let rec unprot_ : type t. t prot_ -> t = fun obj ->
  obj None

let remove_guards g = remove_guards_one prot_merge_ [] g

let unprot :  't. 't prot -> 't = fun g ->
  unprot_ (remove_guards g)

let unprot_send : 't prot -> 't option -> 't = fun g ->
  remove_guards g

type _ seq =
  | Seq : 'hd prot * 'tl seq -> [`seq of 'hd * 'tl] seq
  | SeqGuard : 't seq lazy_t -> 't seq

let rec seq_head : type hd tl. [`seq of hd * tl] seq -> hd prot =
  function
  | Seq(hd,_) -> hd
  | SeqGuard xs -> Guarded [lazy (seq_head (Lazy.force xs))]

let rec seq_tail : type hd tl. [`seq of hd * tl] seq -> tl seq = fun xs ->
  match xs with
  | Seq(_,tl) -> tl
  | SeqGuard xs -> SeqGuard(lazy (seq_tail (Lazy.force xs)))
                 
let rec remove_guards_seq : type t. t seq lazy_t list -> t seq lazy_t -> t seq =
  fun acc w ->
  if find_physeq acc w then
    raise UnguardedLoop
  else begin match Lazy.force w with
       | SeqGuard w' ->
          remove_guards_seq (w::acc) w'
       | w -> w
       end

type (_,_,_,_) lens =
  | Zero  : ('hd0, 'hd1, [`seq of 'hd0 * 'tl] seq, [`seq of 'hd1 * 'tl] seq) lens
  | Succ : ('a, 'b, 'tl0 seq, 'tl1 seq) lens
           -> ('a,'b, [`seq of 'hd * 'tl0] seq, [`seq of 'hd * 'tl1] seq) lens

let rec get : type a b xs ys. (a, b, xs, ys) lens -> xs -> a prot = fun ln xs ->
  match ln with
  | Zero -> seq_head xs
  | Succ ln' -> get ln' (seq_tail xs)
              
let rec put : type a b xs ys. (a,b,xs,ys) lens -> xs -> b prot -> ys =
  fun ln xs b ->
  match ln with
  | Zero -> Seq(b, seq_tail xs)
  | Succ ln' -> Seq(seq_head xs, put ln' (seq_tail xs) b)

let rec seq_merge : type t. t seq -> t seq -> t seq =
  fun ls rs ->
  match ls, rs with
  | Seq(hd_l,tl_l), Seq(hd_r, tl_r) ->
     Seq(prot_merge hd_l hd_r,
         seq_merge tl_l tl_r)
  | SeqGuard(xs), Seq(hd_r, tl_r) ->
     Seq(prot_merge (Guarded [lazy (seq_head (Lazy.force xs))]) hd_r,
         seq_merge (SeqGuard(lazy (seq_tail (Lazy.force xs)))) tl_r)
  | (Seq(_,_) as xs), (SeqGuard(_) as ys) ->
     seq_merge ys xs
  | SeqGuard(w1), w2 ->
     SeqGuard
       (let rec w =
          (lazy begin
               try
                 let w1 = remove_guards_seq [w] w1 in
                 seq_merge w1 w2
               with
                 UnguardedLoop -> w2
             end)
        in w)

let goto : type t. t seq lazy_t -> t seq = fun xs ->
  SeqGuard xs

type ('robj,'c,'a,'b,'xs,'ys) role =
  {label:('robj,'c) method_;
   lens:('a,'b,'xs,'ys) lens}

type close = Close

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

