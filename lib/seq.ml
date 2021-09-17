
let closed = State.make (fun _ _ -> ()) (fun _ _ -> ()) ()

type _ t =
  | SeqCons : 'hd State.t * 'tl t -> [ `cons of 'hd * 'tl ] t
  | SeqNil : ([ `cons of unit * 'a ] as 'a) t

let head : type hd tl. [`cons of hd * tl] t -> hd State.t =
  function
  | SeqCons(hd,_) -> hd
  | SeqNil -> closed

let tail : type hd tl. [`cons of hd * tl] t -> tl t =
  function
  | SeqCons(_,tl) -> tl
  | SeqNil -> SeqNil

let rec get : type a b xs ys. (a, b, xs, ys) Types.idx -> xs t -> a State.t = fun ln xs ->
  match ln with
  | Zero -> head xs
  | Succ ln' -> get ln' (tail xs)

let rec put : type a b xs ys. (a,b,xs,ys) Types.idx -> xs t -> b State.t -> ys t =
  fun ln xs b ->
  match ln with
  | Zero -> SeqCons(b, tail xs)
  | Succ ln' -> SeqCons(head xs, put ln' (tail xs) b)

let rec merge : type x. x t -> x t -> x t = fun l r ->
  match l,r with
  | SeqCons(_,_), _ ->
    let hd = State.merge (head l) (head r) in
    let tl = merge (tail l) (tail r) in
    SeqCons(hd, tl)
  | _, SeqCons(_,_) -> merge r l
  | SeqNil, SeqNil -> SeqNil
