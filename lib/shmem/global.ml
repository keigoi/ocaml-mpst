open Mpst_base

module Make(F:S.FLAG)(L:S.LIN) = struct
  module Lin = L
  module Sess = Session.Make(F)
  open Sess

type 'a one = One__ of 'a
type 'a many = Many__ of 'a
type 'a lin = 'a Lin.lin

type _ e =
  (* slot contents *)
  One : 'a sess lazy_t -> 'a sess one e
| Many : 'a sess lazy_t list -> 'a sess many e

let sess_ p =
  {once=Flag.create();
   prot=p}

let sess p =
  Lazy.from_val @@ sess_ p

let unone : type t. t sess one e -> t sess = function
    One p -> Lazy.force p

let unsess_one p =
  match unone p with {prot; _} -> prot

let unsess {prot; _} = prot

let unmany : type t. t sess many e -> t sess lazy_t list = function
    Many p -> p

include Mpst_base.Lens.Make(struct type 't u = 't e end)

type ('r, 'v1, 'v2, 's1, 's2) role =
  {role:'r;
   lens:('v1, 'v2, 's1, 's2) lens;
   }

type ('la,'lb,'ca,'cb,'v1, 'v2) label =
    {select_label: ('v1 -> 'ca) -> 'la;
     offer_label: 'v2 * 'cb -> 'lb}

let (-->) : type ra rb sa sb la lb c0 c1 c2 v.
      (ra, sa sess one, (rb, la) send sess one, c0, c1) role ->
      (rb, sb sess one, (ra, lb) receive sess one, c1, c2) role ->
      (la, lb, sa sess lin, sb sess lin, v, v) label ->
      c0 lazy_t -> c2 lazy_t =
  fun a b ({select_label;offer_label}) c0 ->
  let st, push = Lwt_stream.create () in
  let sa = lens_get a.lens c0 in
  let sa =
    Lazy.from_val @@
      One (sess @@  Send (b.role,
                          select_label (fun v ->
                              push (Some v);
                              Lin.mklin @@
                                {once=Flag.create();
                                 prot=unsess_one (Lazy.force sa)})))
  in
  let c1 = lens_put a.lens c0 sa in
  let sb = lens_get b.lens c1 in
  let sb =
    Lazy.from_val @@
      One (sess @@
             Receive (a.role,
                      (fun () -> [
                           Lwt_stream.next st |> Lwt.map (fun v ->
                           offer_label (v, Lin.mklin @@ {once=Flag.create(); prot=unsess_one (Lazy.force sb)}))])))
  in
  let c2 = lens_put b.lens c1 sb in
  c2

(* broadcast *)
let (-->>) : type ra rb sa sb la lb c0 c1 c2 v.
      (ra, sa sess one, (rb, la) send sess one, c1, c2) role ->
      (rb, sb sess many, (ra, lb) receive sess many, c0, c1) role ->
      (la, lb, sa sess lin, sb sess lin, int -> v, v) label ->
      c0 lazy_t -> c2 lazy_t =
  fun a b ({select_label;offer_label}) c0 ->
  let sbs = lens_get b.lens c0 in
  let sbs =
    lazy
      (List.map
         (fun sb ->
           let st, push = Lwt_stream.create () in
           sess @@
             Receive (a.role, (fun () -> [
                 Lwt_stream.next st |> Lwt.map (fun v ->
                 offer_label (v, Lin.mklin @@ {once=Flag.create(); prot=unsess @@ Lazy.force sb}))
             ])),
           push
         )
         (unmany (Lazy.force sbs)))
  in
  let c1 = lens_put b.lens c0 (lazy (Many (List.map fst (Lazy.force sbs)))) in
  let sa = lens_get a.lens c1 in
  let sa =
    Lazy.from_val @@
      One (sess @@ Send (b.role,
            select_label (fun f ->
                List.iteri
                  (fun i (_,push) -> push (Some (f i)))
                  (Lazy.force sbs);
                Lin.mklin @@ {once=Flag.create();  prot=unsess_one (Lazy.force sa)})))
  in
  let c2 = lens_put a.lens c1 sa in
  c2

(* gather *)
let (>>--) : type ra rb sa sb la lb c0 c1 c2 v.
      (ra, sa sess many, (rb, la) send sess many, c0, c1) role ->
      (rb, sb sess one, (ra, lb) receive sess one, c1, c2) role ->
      (la, lb, sa sess lin, sb sess lin, v, v list) label ->
      c0 lazy_t -> c2 lazy_t =
  fun a b ({select_label;offer_label}) c0 ->
  let sas = lens_get a.lens c0 in
  let sas =
    lazy
      (List.map
         (fun sa ->
           let st, push = Lwt_stream.create () in
           sess @@
             Send (b.role,
                 select_label (fun v -> push (Some v); Lin.mklin {once=Flag.create(); prot=unsess @@ Lazy.force sa})),
           st
         )
      (unmany (Lazy.force sas)))
  in
  let c1 = lens_put a.lens c0 (lazy (Many (List.map fst (Lazy.force sas)))) in
  let sb = lens_get b.lens c1 in
  let sb =
    lazy begin
      One (sess @@ Receive (a.role, (fun () -> [
            let sts = List.map snd (Lazy.force sas) in
            let vs = Lwt_list.map_s Lwt_stream.next sts in
            Lwt.map
              (fun v -> offer_label (v, Lin.mklin {once=Flag.create(); prot=unsess_one (Lazy.force sb)}))
              vs
        ]))) end
  in
  let c2 = lens_put b.lens c1 sb in
  c2

let dummy_receive ra c0 =
  lens_put ra.lens c0 (Lazy.from_val (One (sess DummyReceive)))

let dummy_close ra c0 =
  lens_put ra.lens c0 (Lazy.from_val (One (sess Close)))

type ('l, 'r, 'lr) label_merge =
    {label_merge: 'l -> 'r -> 'lr}

let label : type l ks k r. (r, l) send sess one e lazy_t -> l =
  function
  | lazy (One (lazy {prot=Send (_, l); _})) -> l
  | lazy (One (lazy {prot=Close})) -> assert false

let role : type l ks k r. (r, l) send sess one e lazy_t -> r =
  function
  | lazy (One (lazy {prot=Send (r, _); _})) -> r
  | lazy (One (lazy {prot=Close; _})) -> assert false

let lf = Lazy.force

let lv = Lazy.from_val

let rec merge__ : type t. t e -> t e -> t e = fun l r ->
  match l,r with
  | One(l), One(r) ->
     One(lazy (sess_ (Internal.merge (lf l).prot (lf r).prot)))
  | Many(ls), Many(rs) ->
     Many(List.map2 (fun x y -> lazy (sess_ @@ Internal.merge (lf x).prot (lf y).prot)) ls rs)

let rec merge_ : type t. t slots lazy_t -> t slots lazy_t -> t slots lazy_t =
  fun l r ->
  lazy begin
      match l, r with
      | lazy (Cons(hd_l,tl_l)), lazy (Cons(hd_r, tl_r)) ->
         (Cons(lazy (merge__ (lf hd_l) (lf hd_r)), merge_ tl_l tl_r))
      | lazy Nil, _ ->
         Nil
    end

let choice_at a {label_merge} (al,cl) (ar,cr) =
  let sal, sar = lens_get al.lens cl, lens_get ar.lens cr in
  let cl, cr = lens_put_ al.lens cl (One (sess Close)),
               lens_put_ ar.lens cr (One (sess Close)) in
  let c = merge_ cl cr in
  let lr = lv (One (sess (Send (role sar, label_merge (label sal) (label sar))))) in
  lens_put a.lens c lr

let loop c0 = lazy (Lazy.force (Lazy.force c0))

let get_sess r m =
  let p = lens_get_ r.lens m in
  match p with One p -> lf p

let get_sess_many r m =
  let p = lens_get_ r.lens m in
  match p with Many ps -> List.map Lazy.force ps

let lv = Lazy.from_val


let nil = lv Nil
let one tl = lv @@ Cons(lv @@ One (sess Close), tl)

let many_at (role: ('r, close sess one, close sess many, 'c0, 'c1) role) num (cont:'c0 lazy_t) =
  let cs =
    repeat num (fun i -> lazy (sess_ Close))
  in
  lens_put role.lens cont (lazy (Many cs))


let many_at_ role num cont =
  let cs =
    repeat num (fun i ->
        lazy (Lazy.force @@ List.nth (unmany @@ lens_get_ role.lens cont) i))
  in
  lens_put role.lens cont (lazy (Many cs))
end
