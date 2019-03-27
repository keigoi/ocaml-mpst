type close = unit

type _ sess =
  | Send : (< .. > as 'obj) -> 'obj sess
  | Recv : ([>] as 'var) Event.event -> 'var Event.event sess
  | Close : close sess

let unsess : type t. t sess -> t = function
  | Send(obj) -> obj
  | Recv(f) -> f
  | Close -> ()

let close () =
  ()

type ('la,'lb,'va,'vb) label =
    {make_obj: 'va -> 'la;
     make_var: 'vb -> 'lb}

type _ eps =
  Cons : 'x sess * 'xs eps lazy_t -> ('x * 'xs) eps
| Nil : unit eps

type (_,_,_,_) lens =
  | Fst  : ('a, 'b, ('a * 'xs) eps, ('b * 'xs) eps) lens
  | Next : ('a,'b, 'xs eps,'ys eps) lens
           -> ('a,'b, ('x * 'xs) eps, ('x * 'ys) eps) lens
(* type _ eps =
 *   Cons : 'x sess * 'xs eps lazy_t -> ([`o of 'x * 'xs]) eps
 * | Nil : unit eps
 * 
 * type (_,_,_,_) lens =
 *   | Fst  : ('a, 'b, ([`o of 'a * 'xs]) eps, ([`o of 'b * 'xs]) eps) lens
 *   | Next : ('a,'b, 'xs eps,'ys eps) lens
 *            -> ('a,'b, ([`o of 'x * 'xs]) eps, ([`o of 'x * 'ys]) eps) lens *)
            

let slot_head : type hd tl. (hd * tl) eps lazy_t -> hd sess = fun sl ->
  match sl with
  | lazy (Cons(hd,_)) -> hd

let slot_tail : type hd tl. (hd * tl) eps lazy_t -> tl eps lazy_t = fun sl ->
  lazy begin
      match sl with
      | lazy (Cons(_,lazy tl)) -> tl
    end
  
let rec lens_get : type a b xs ys. (a, b, xs, ys) lens -> xs lazy_t -> a sess = fun ln xs ->
  match ln with
  | Fst -> slot_head xs
  | Next ln' -> lens_get ln' (slot_tail xs)

let rec lens_put : type a b xs ys. (a,b,xs,ys) lens -> xs lazy_t -> b sess -> ys lazy_t =
  fun ln xs b ->
  match ln with
  | Fst -> lazy (Cons(b, slot_tail xs))
  | Next ln' ->
     lazy
       begin match xs with
       | lazy (Cons(a, xs')) -> Cons(a, lens_put ln' xs' b)
       end

type ('robj,'rvar,'c,'a,'b,'xs,'ys) role = {role:('robj,'rvar,'c,'c) label; lens:('a,'b,'xs,'ys) lens}

let comm a b label g0 =
  let ch = Event.new_channel () in
  let obj =
    b.role.make_obj @@
      label.make_obj
        (fun v ->
          Event.sync (Event.send ch v);
          unsess (lens_get a.lens g0)) in
  let g1 = lens_put a.lens g0 (Send obj) in
  let ev =
    let ev = Event.receive ch in
    Event.wrap ev (fun v -> a.role.make_var @@ label.make_var (v, unsess (lens_get b.lens g1))) in
  let g2 = lens_put b.lens g1 (Recv ev) in
  g2

let (-->) = comm

exception RoleNotEnabled
  
let merge_sess : type s. s sess -> s sess -> s sess = fun l r ->
  match l, r with
  | Send _, Send _ -> raise RoleNotEnabled
  | Recv f, Recv g -> Recv (Event.choose [f; g])
  | Close, Close -> Close
  | Close, Recv _ -> assert false
  | Recv _, Close -> assert false
  | Recv _, Send _ -> assert false
  | Send _, Recv _ -> assert false
                    
  
let rec merge_eps : type t. t eps lazy_t -> t eps lazy_t -> t eps lazy_t = fun ls rs ->
  lazy begin
      match ls, rs with
      | lazy (Cons(hd_l,tl_l)), lazy (Cons(hd_r, tl_r)) ->
         (Cons(merge_sess hd_l hd_r, merge_eps tl_l tl_r))
      | lazy Nil, _ ->
         Nil
    end

type ('l, 'r, 'lr) obj_merge =
    {obj_merge: 'l -> 'r -> 'lr}

let send_obj : 'obj. (< .. > as 'obj) sess -> 'obj = function[@warning "-8"]
  | Send obj -> obj
            

let choice_at a merge (al,cl) (ar,cr) =
  let sal, sar = lens_get al.lens cl, lens_get ar.lens cr in
  let cl, cr = lens_put al.lens cl Close,
               lens_put ar.lens cr Close in
  let c = merge_eps cl cr in
  let lr = Send (merge.obj_merge (send_obj sal) (send_obj sar)) in
  lens_put a.lens c lr

let goto l =
  lazy (Lazy.force @@ Lazy.force l)

let a = {role={make_obj=(fun v->object method role_a=v end);
               make_var=(fun v->(`role_a(v):[`role_a of _]))}; (* explicit annotataion is mandatory *)
         lens=Fst}
let b = {role={make_obj=(fun v->object method role_b=v end);
               make_var=(fun v->(`role_b(v):[`role_b of _]))}; (* explicit annotataion is mandatory *)
         lens=Next Fst}
let c = {role={make_obj=(fun v->object method role_c=v end);
               make_var=(fun v->(`role_c(v):[`role_c of _]))}; (* explicit annotataion is mandatory *)
         lens=Next (Next Fst)}
let msg =
  {make_obj=(fun f -> object method msg=f end);
   make_var=(fun v -> `msg(v))}
let left =
  {make_obj=(fun f -> object method left=f end);
   make_var=(fun v -> `left(v))}
let right =
  {make_obj=(fun f -> object method right=f end);
   make_var=(fun v -> `right(v))}
let left_or_right =
  {obj_merge=(fun l r -> object method left=l#left method right=r#right end)}
let to_b m =
  {obj_merge=(fun l r -> object method role_b=m.obj_merge l#role_b r#role_b end)}
let b_or_c =
  {obj_merge=(fun l r -> object method role_b=l#role_b method role_c=r#role_c end)}
  
(* let finish =
 *   let rec fini = lazy (Cons(Close, fini)) in
 *   Lazy.from_val (Lazy.force fini) *)

let get_sess r g = unsess (lens_get r.lens g)

let one xs = Lazy.from_val (Cons(Close, xs))
let nil = Lazy.from_val Nil

let finish = one @@ one @@ one @@ nil
