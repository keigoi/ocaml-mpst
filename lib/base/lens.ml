module Make(X:sig type 't u end) = struct
  open X
         
  type _ slots =
    Cons : 'x u lazy_t * 'xs slots lazy_t -> ('x * 'xs) slots
  | Nil : unit slots

  type (_,_,_,_) lens =
    | Fst  : ('a, 'b, ('a * 'xs) slots, ('b * 'xs) slots) lens
    | Next : ('a,'b, 'xs slots,'ys slots) lens
             -> ('a,'b, ('x * 'xs) slots, ('x * 'ys) slots) lens
              
  let slot_tail : type hd tl. (hd * tl) slots lazy_t -> tl slots lazy_t = fun sl ->
    lazy begin
        match sl with
        | lazy (Cons(_,lazy tl)) -> tl
      end

  let slot_head : type hd tl. (hd * tl) slots lazy_t -> hd u lazy_t = fun sl ->
    lazy begin
        match sl with
        | lazy (Cons(hd,_)) -> Lazy.force hd
      end
    
  let rec lens_get : type a b xs ys. (a, b, xs, ys) lens -> xs lazy_t -> a u lazy_t = fun ln xs ->
    match ln with
    | Fst -> slot_head xs
    | Next ln' -> lens_get ln' (slot_tail xs)

  let lens_get_ ln s = Lazy.force (lens_get ln s)

  let rec lens_put : type a b xs ys. (a,b,xs,ys) lens -> xs lazy_t -> b u lazy_t -> ys lazy_t =
    fun ln xs b ->
    match ln with
    | Fst -> lazy (Cons(b, slot_tail xs))
    | Next ln' ->
       lazy
         begin match xs with
         | lazy (Cons(a, xs')) -> Cons(a, lens_put ln' xs' b)
         end

  let lens_put_ ln s v = lens_put ln s (Lazy.from_val v)
end
