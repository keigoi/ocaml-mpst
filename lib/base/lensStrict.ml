type _ slots =
  Cons : 'x * 'xs slots -> ('x * 'xs) slots
| Nil : unit slots

type (_,_,_,_) lens =
  | Fst  : ('a, 'b, ('a * 'xs) slots, ('b * 'xs) slots) lens
  | Next : ('a,'b, 'xs slots,'ys slots) lens
           -> ('a,'b, ('x * 'xs) slots, ('x * 'ys) slots) lens
  
let slot_tail : type hd tl. (hd * tl) slots -> tl slots = fun sl ->
  match sl with
  | Cons(_, tl) -> tl

let slot_head : type hd tl. (hd * tl) slots -> hd = fun sl ->
  match sl with
  | Cons(hd,_) -> hd
                
let rec lens_get : type a b xs ys. (a, b, xs, ys) lens -> xs -> a = fun ln xs ->
  match ln with
  | Fst -> slot_head xs
  | Next ln' -> lens_get ln' (slot_tail xs)

let rec lens_put : type a b xs ys. (a,b,xs,ys) lens -> xs -> b -> ys =
  fun ln xs b ->
  match ln with
  | Fst -> Cons(b, slot_tail xs)
  | Next ln' ->
     match xs with
     | Cons(a, xs') -> Cons(a, lens_put ln' xs' b)
