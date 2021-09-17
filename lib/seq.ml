
let closed = State.make (fun _ _ -> ()) (fun _ _ -> ()) ()

type _ t =
  | SeqCons : 'hd State.t * 'tl t -> [ `cons of 'hd * 'tl ] t
  | SeqNil : ([ `cons of unit * 'a ] as 'a) t

(* val seq_head : [ `cons of 'hd * 'tl ] t -> 'hd state
val seq_tail : [ `cons of 'hd * 'tl ] t -> 'tl t
val get : ('a, 'b, 'xs, 'ys) Types.idx -> 'xs t -> 'a state

val put :
  ('a, 'b, 'xs, 'ys) Types.idx -> 'xs t -> 'b state -> 'ys t *)

let seq_head : type hd tl. [`cons of hd * tl] t -> hd State.t =
  function
  | SeqCons(hd,_) -> hd
  | SeqNil -> closed

let seq_tail : type hd tl. [`cons of hd * tl] t -> tl t =
  function
  | SeqCons(_,tl) -> tl
  | SeqNil -> SeqNil

let rec get : type a b xs ys. (a, b, xs, ys) Types.idx -> xs t -> a State.t = fun ln xs ->
  match ln with
  | Zero -> seq_head xs
  | Succ ln' -> get ln' (seq_tail xs)

let rec put : type a b xs ys. (a,b,xs,ys) Types.idx -> xs t -> b State.t -> ys t =
  fun ln xs b ->
  match ln with
  | Zero -> SeqCons(b, seq_tail xs)
  | Succ ln' -> SeqCons(seq_head xs, put ln' (seq_tail xs) b)


let rec seq_merge : type x. x t -> x t -> x t = fun l r ->
  match l,r with
  | SeqCons(_,_), _ ->
    let hd = State.merge (seq_head l) (seq_head r) in
    let tl = seq_merge (seq_tail l) (seq_tail r) in
    SeqCons(hd, tl)
  | _, SeqCons(_,_) -> seq_merge r l
  | SeqNil, SeqNil -> SeqNil
