type (_,_,_,_) lens =
  | Fst  : ('a,'b,'a lazy_t * 'xs, 'b * 'xs) lens
  | Next : ('a,'b,'xs,'ys) lens -> ('a,'b,'x * 'xs, 'x * 'ys) lens
  | Any  : ('d1 -> 'a) * ('d1 -> 'b -> 'd2) -> ('a, 'b, 'd1, 'd2) lens

let rec lget : type a b xs ys. (a, b, xs, ys) lens -> xs -> a = fun ln xs ->
  match ln,xs with
  | Fst, (a,_)        -> Lazy.force a
  | Next ln', (_,xs') -> lget ln' xs'
  | Any (get, _), xs  -> get xs

let lens_get l s = lget l (Lazy.force s)
                       
let rec lput : type a b xs ys. (a,b,xs,ys) lens -> xs -> b -> ys = fun ln xs b ->
  match ln, xs with
  | Fst, (_, xs) -> (b, xs)
  | Next ln', (a, xs') -> (a, lput ln' xs' b)
  | Any (_, put), xs -> put xs b

let lens_put l s v = lazy (lput l (Lazy.force s) v)
                      
let _0 = Fst;;  let _1 = Next _0;; let _2 = Next _1;; let _3 = Next _2
