
let map_option f = function
  | Some x -> Some (f x)
  | None -> None

type ('la,'lb,'va,'vb) label =
  {make_obj: 'va -> 'la;
   call_obj: 'la -> 'va;
   make_var: 'vb -> 'lb}

type ('lr, 'l, 'r) obj_merge =
  {obj_merge: 'l -> 'r -> 'lr;
   obj_splitL: 'lr -> 'l;
   obj_splitR: 'lr -> 'r;
  }

(* type close = Close *)

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

module Seq(X:PARAM) = struct
  type param = X.t

  type 'p prot_ =
    (param -> 'p) option -> param -> 'p

  let prot_merge_ : type s. s prot_ -> s prot_ -> s prot_ = fun l r ->
    fun obj -> l (Some (r obj))

  type 'a prot = 'a prot_ guarded

  let prot_merge l r = guarded_merge prot_merge_ l r

  let rec unprot_ : type t. param -> t prot_ -> t = fun kt obj ->
    obj None kt

  let unprot : param -> 't prot -> 't = fun kt g ->
    unprot_ kt (remove_guards_one prot_merge_ [] g)

  let unprot_send : 'a prot -> (param -> 'a) option -> param -> 'a = fun g ->
    remove_guards_one prot_merge_ [] g
                   
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

  type close = <close : unit >
  let protclose _ _ = object method close = () end

  let finish : ([`seq of close * 'a] as 'a) seq =
    let rec loop = lazy (Seq(val_ @@ protclose, SeqGuard(loop))) in
    Lazy.force loop

  type ('robj,'rvar,'c,'a,'b,'xs,'ys) role =
    {label:('robj,'rvar,'c,'c) label;
     lens:('a,'b,'xs,'ys) lens}
  
  let choice_at : 'ep 'ep_l 'ep_r 'g0_l 'g0_r 'g1 'g2.
                    (_, _, _, close, < .. > as 'ep, 'g1 seq, 'g2 seq) role ->
                  ('ep, < .. > as 'ep_l, < .. > as 'ep_r) obj_merge ->
                  (_, _, _, 'ep_l, close, 'g0_l seq, 'g1 seq) role * 'g0_l seq ->
                  (_, _, _, 'ep_r, close, 'g0_r seq, 'g1 seq) role * 'g0_r seq ->
                  'g2 seq
    = fun r merge (r',g0left) (r'',g0right) ->
    let epL, epR = get r'.lens g0left, get r''.lens g0right in
    let g1left, g1right =
      put r'.lens g0left (val_ protclose), put r''.lens g0right (val_ protclose) in
    let g1 = seq_merge g1left g1right in
    let ep =
      val_ @@ (fun obj kt ->
                  let oleft, oright = unprot_send epL, unprot_send epR in
                  let oleft = oleft (map_option (fun obj kt -> merge.obj_splitL (obj kt)) obj) kt
                  and oright = oright (map_option (fun obj kt -> merge.obj_splitR (obj kt)) obj) kt in
                  merge.obj_merge oleft oright)
    in
    let g2 = put r.lens g1 ep
    in
    g2

  let a = {label={make_obj=(fun v->object method role_A=v end);
                  call_obj=(fun o->o#role_A);
                  make_var=(fun v->(`role_A(v):[`role_A of _]))};
           lens=Zero}
  let b = {label={make_obj=(fun v->object method role_B=v end);
                  call_obj=(fun o->o#role_B);
                  make_var=(fun v->(`role_B(v):[`role_B of _]))};
           lens=Succ Zero}
  let c = {label={make_obj=(fun v->object method role_C=v end);
                  call_obj=(fun o->o#role_C);
                  make_var=(fun v->(`role_C(v):[`role_C of _]))};
           lens=Succ (Succ Zero)}
  let d = {label={make_obj=(fun v->object method role_D=v end);
                  call_obj=(fun o->o#role_D);
                  make_var=(fun v->(`role_D(v):[`role_D of _]))};
           lens=Succ (Succ (Succ Zero))}
end
    
module type LIN = sig
  type 'a lin
  val mklin : 'a -> 'a lin
  val unlin : 'a lin -> 'a
end

let msg =
  {make_obj=(fun f -> object method msg=f end);
   call_obj=(fun o -> o#msg);
   make_var=(fun v -> `msg(v))}
let left =
  {make_obj=(fun f -> object method left=f end);
   call_obj=(fun o -> o#left);
   make_var=(fun v -> `left(v))}
let right =
  {make_obj=(fun f -> object method right=f end);
   call_obj=(fun o -> o#right);
   make_var=(fun v -> `right(v))}
let middle =
  {make_obj=(fun f -> object method middle=f end);
   call_obj=(fun o -> o#middle);
   make_var=(fun v -> `middle(v))}
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

