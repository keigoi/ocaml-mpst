
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

  let merge0 : type t. t merge__ -> t merge__ -> t merge__ = fun l r ->
    fun obj -> l (Some (r obj))

  let merge : 'a. 'a t -> 'a t -> 'a t =
    fun l r ->
    match l, r with
    | Val_ ll, Val_ rr ->
       Val_ (merge0 ll rr)
    | Guarded_ g1, Guarded_ g2 ->
       Guarded_ (g1 @ g2)
    | ((Val_ _) as b, Guarded_ g) | (Guarded_ g, ((Val_ _) as b)) ->
       Guarded_ ([lazy b] @ g)

  let rec apply : 'a 'b. ('a -> 'b) t -> 'a -> 'b t = fun g v ->
    match g with
    | Guarded_(gs) ->
       Guarded_(List.map (fun g -> lazy (apply (Lazy.force g) v)) gs)
    | Val_ f ->
       Val_ (fun w ->
           match w with
           | Some w -> f (Some (fun _ -> w)) v
           | None -> f None v)

  let remove_one_ : 'a. 'a t -> 'a option -> 'a = fun g ->
    let rec loop hist g =
      match g with
      | Val_ x -> x
      | Guarded_ gs ->
         let solved =
           List.fold_left (fun acc g' ->
               try
                 if find_physeq hist g' then begin
                     acc (* found a loop. drop it *)
                   end else
                   loop (g'::hist) (Lazy.force g') :: acc
               with
                 UnguardedLoop -> acc) [] gs
         in
         match solved with
         | [] ->
            raise UnguardedLoop
         | x::xs ->
            List.fold_left merge0 x xs
    in loop [] g

  let out_ : 'a. 'a t -> 'a = fun g ->
    remove_one_ g None

  let out : 'a. 'a t -> 'a option -> 'a = fun g ->
    remove_one_ g
    
  let no_merge : 'a. 'a -> 'a t  = fun v ->
    Val_ (fun _ -> v)
    
  let bare_ : 'a. ('a option -> 'a) -> 'a t  = fun v ->
    Val_ v

  let guarded : 'a. 'a t lazy_t list -> 'a t  = fun v ->
    Guarded_ v

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
  | SeqGuard : 't seq lazy_t -> 't seq

let rec seq_head : type hd tl. [`cons of hd * tl] seq -> hd Mergeable.t =
  function
  | Seq(hd,_) -> hd
  | SeqGuard xs -> Mergeable.guarded [lazy (seq_head (Lazy.force xs))]

let rec seq_tail : type hd tl. [`cons of hd * tl] seq -> tl seq = fun xs ->
  match xs with
  | Seq(_,tl) -> tl
  | SeqGuard xs -> SeqGuard(lazy (seq_tail (Lazy.force xs)))

let rec remove_guards_seq : type t. t seq lazy_t list -> t seq lazy_t -> t seq =
  fun acc w ->
  if find_physeq acc w then
    raise Mergeable.UnguardedLoop
  else begin match Lazy.force w with
       | SeqGuard w' ->
          remove_guards_seq (w::acc) w'
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

let rec seq_merge : type t. t seq -> t seq -> t seq =
  fun ls rs ->
  match ls, rs with
  | Seq(hd_l,tl_l), Seq(hd_r, tl_r) ->
     Seq(Mergeable.merge hd_l hd_r,
         seq_merge tl_l tl_r)
  | SeqGuard(xs), Seq(hd_r, tl_r) ->
     Seq(Mergeable.merge (Mergeable.guarded [lazy (seq_head (Lazy.force xs))]) hd_r,
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
                 Mergeable.UnguardedLoop -> w2
             end)
        in w)

let goto : type t. t seq lazy_t -> t seq = fun xs ->
  SeqGuard xs

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
