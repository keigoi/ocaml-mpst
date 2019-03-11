open Session
 
type 'a one = One__ of 'a       
type 'a many = Many__ of 'a       
type conn = Conn

type _ e =
  (* slot contents *)
  One : 'a prot lazy_t -> 'a prot one e
| Many : 'a prot lazy_t list -> 'a prot many e

let unone : type t. t prot one e -> t prot = function
    One p -> Lazy.force p

let unmany : type t. t prot many e -> t prot lazy_t list = function
    Many p -> p
  
include Lens.Make(struct type 't u = 't e end)
            
type ('r, 'v1, 'v2, 's1, 's2) role =
  {role:'r;
   lens:('v1, 'v2, 's1, 's2) lens;
   }

type ('la,'lb,'ca,'cb,'v1, 'v2) label =
    {select_label: ('v1 -> 'ca) -> 'la;
     offer_label: 'v2 * 'cb -> 'lb}

let (-->) : type ra rb sa sb la lb c0 c1 c2 v.
      (ra, sa prot one, (rb, la) send prot one, c0, c1) role ->
      (rb, sb prot one, (ra, lb) receive prot one, c1, c2) role ->
      (la, lb, sa prot, sb prot, v, v) label ->
      c0 lazy_t -> c2 lazy_t =
  fun a b ({select_label;offer_label}) c0 ->
  let st, push = Lwt_stream.create () in
  let sa = lens_get a.lens c0 in
  let sa =
    Lazy.from_val @@
      One (Lazy.from_val @@ Send (b.role, 
            select_label (fun v -> push (Some v); unone (Lazy.force sa))))
  in
  let c1 = lens_put a.lens c0 sa in
  let sb = lens_get b.lens c1 in
  let sb =
    Lazy.from_val @@
      One (Lazy.from_val @@ Receive (a.role, (fun () -> [
            Lwt.map (fun v -> offer_label (v, unone (Lazy.force sb))) (Lwt_stream.next st)])))
  in
  let c2 = lens_put b.lens c1 sb in
  c2

(* broadcast *)
let (-->>) : type ra rb sa sb la lb c0 c1 c2 v.
      (ra, sa prot one, (rb, la) send prot one, c1, c2) role ->
      (rb, sb prot many, (ra, lb) receive prot many, c0, c1) role ->
      (la, lb, sa prot, sb prot, int -> v, v) label ->
      c0 lazy_t -> c2 lazy_t =
  fun a b ({select_label;offer_label}) c0 ->
  let sbs = lens_get b.lens c0 in
  let sbs =
    lazy
      (List.map
         (fun sb ->
           let st, push = Lwt_stream.create () in
           Lazy.from_val @@
             Receive (a.role, (fun () -> [
                 Lwt_stream.next st |>
                 Lwt.map (fun v -> offer_label (v, Lazy.force sb))
             ])),
           push
         )
         (unmany (Lazy.force sbs)))
  in
  let c1 = lens_put b.lens c0 (lazy (Many (List.map fst (Lazy.force sbs)))) in
  let sa = lens_get a.lens c1 in
  let sa =
    Lazy.from_val @@
      One (Lazy.from_val @@ Send (b.role, 
            select_label (fun f ->
                List.iteri
                  (fun i (_,push) -> push (Some (f i)))
                  (Lazy.force sbs);
                unone (Lazy.force sa))))
  in
  let c2 = lens_put a.lens c1 sa in
  c2

(* gather *)
let (>>--) : type ra rb sa sb la lb c0 c1 c2 v.
      (ra, sa prot many, (rb, la) send prot many, c0, c1) role ->
      (rb, sb prot one, (ra, lb) receive prot one, c1, c2) role ->
      (la, lb, sa prot, sb prot, v, v list) label ->
      c0 lazy_t -> c2 lazy_t =
  fun a b ({select_label;offer_label}) c0 ->
  let sas = lens_get a.lens c0 in
  let sas =
    lazy
      (List.map
         (fun sa ->
           let st, push = Lwt_stream.create () in
           Lazy.from_val @@
             Send (b.role, 
                 select_label (fun v -> push (Some v); Lazy.force sa)),
           st
         )
      (unmany (Lazy.force sas)))
  in
  let c1 = lens_put a.lens c0 (lazy (Many (List.map fst (Lazy.force sas)))) in
  let sb = lens_get b.lens c1 in
  let sb =
    lazy begin
      One (Lazy.from_val @@ Receive (a.role, (fun () -> [
            let sts = List.map snd (Lazy.force sas) in
            let vs = Lwt_list.map_s Lwt_stream.next sts in
            Lwt.map
              (fun v -> offer_label (v, unone (Lazy.force sb)))
              vs
        ]))) end
  in
  let c2 = lens_put b.lens c1 sb in
  c2

let dummy_receive ra c0 =
  lens_put ra.lens c0 (Lazy.from_val (One (Lazy.from_val DummyReceive)))

let dummy_close ra c0 =
  lens_put ra.lens c0 (Lazy.from_val (One (Lazy.from_val Close)))

type ('l, 'r, 'lr) label_merge =
    {label_merge: 'l -> 'r -> 'lr}

let label : type l ks k r. (r, l) send prot one e lazy_t -> l =
  function
  | lazy (One (lazy (Send (_, l)))) -> l
  | lazy (One (lazy Close)) -> assert false

let role : type l ks k r. (r, l) send prot one e lazy_t -> r =
  function
  | lazy (One (lazy (Send (r, _)))) -> r
  | lazy (One (lazy Close)) -> assert false

let lf = Lazy.force

let lv = Lazy.from_val
                      
let rec merge_ : type t. t slots lazy_t -> t slots lazy_t -> t slots lazy_t =
  fun l r ->
  match l, r with
  | lazy (Cons(lazy (One hd_l),tl_l)), lazy (Cons(lazy (One hd_r), tl_r)) ->
     lazy (Cons (lv (One (lazy (Internal.merge (lf hd_l) (lf hd_r)))), merge_ tl_l tl_r))
  | lazy (Cons(lazy (Many hd_l),tl_l)), lazy (Cons(lazy (Many hd_r),tl_r)) ->
     lazy (Cons (lazy (Many (List.rev @@ List.rev_map2 (fun x y -> lazy (Internal.merge (lf x) (lf y))) hd_l hd_r)), merge_ tl_l tl_r))
  | lazy Nil, _ ->
     Lazy.from_val Nil

let choice_at a {label_merge} (al,cl) (ar,cr) =
  let sal, sar = lens_get al.lens cl, lens_get ar.lens cr in
  let cl, cr = lens_put al.lens cl (Lazy.from_val @@ One (lv Close)),
               lens_put ar.lens cr (Lazy.from_val @@ One (lv Close)) in
  let c = merge_ cl cr in
  let lr = lazy (One (lazy (Send (role sar, label_merge (label sal) (label sar))))) in
  lens_put a.lens c lr

let loop c0 = lazy (Lazy.force (Lazy.force c0))

let get_sess r m =
  let p = lens_get_ r.lens m in
  match p with One p -> lf p

let get_sess_many r m =
  let p = lens_get_ r.lens m in
  match p with Many ps -> List.map Lazy.force ps

let lv = Lazy.from_val
let repeat num f =
  let r = ref [] in
  for i=0 to num-1
  do
    r := (f i)::!r
  done;
  !r
  

let nil = lv Nil
let one tl = lv @@ Cons(lv @@ One (lv Close), tl)

let many_at (role: ('r, close prot one, close prot many, 'c0, 'c1) role) num (cont:'c0 lazy_t) =
  let cs =
    repeat num (fun i -> lazy Close)
  in
  lens_put role.lens cont (lazy (Many cs))
                          

let many_at_ role num cont =
  let cs =
    repeat num (fun i ->
        lazy (Lazy.force @@ List.nth (unmany @@ lens_get_ role.lens cont) i))
  in
  lens_put role.lens cont (lazy (Many cs))
