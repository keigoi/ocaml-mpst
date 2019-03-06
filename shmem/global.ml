open Base
open Session

type ('la,'lb,'ca,'cb,'v) label =
    {select_label: ('v -> 'ca) -> 'la;
     offer_label: 'v * 'cb -> 'lb}

let (-->) : type ra rb sa sb la lb c0 c1 c2 v.
      (ra, sa, (rb, la) send, c0, c1) role ->
      (rb, sb, (ra, lb) receive, c1, c2) role ->
      (la, lb, sa prot, sb prot, v) label ->
      c0 lazy_t -> c2 lazy_t =
  fun a b ({select_label;offer_label}) c0 ->
  let st, push = Lwt_stream.create () in
  let sa = lazy (lens_get a.lens c0) in
  let sa =
    Send (b.role, 
          select_label (fun v -> push (Some v); Lazy.force sa))
  in
  let c1 = lens_put a.lens c0 sa in
  let sb = lazy (lens_get b.lens c1) in
  let sb =
    Receive (a.role, [
            Lwt.map (fun v -> offer_label (v, Lazy.force sb)) (Lwt_stream.next st)])
  in
  let c2 = lens_put b.lens c1 sb in
  c2


let dummy_receive ra c0 =
  lens_put ra.lens c0 DummyReceive

let dummy_close ra c0 =
  lens_put ra.lens c0 Close

type ('l, 'r, 'lr) label_merge =
    {label_merge: 'l -> 'r -> 'lr}

let label : type l ks k r. (r, l) send prot -> l =
  function
  | Send (_, l) -> l
  | Close -> assert false

let role : type l ks k r. (r, l) send prot -> r =
  function
  | (Send (r, _)) -> r
  | Close -> assert false

let rec merge_ : type t. t slots -> t slots -> t slots =
  fun l r ->
  match l, r with
  | Cons(lazy hd_l,tl_l), Cons(lazy hd_r,tl_r) ->
     Cons (lazy (Internal.merge hd_l hd_r), merge_ tl_l tl_r)
  | Nil, Nil ->
     Nil

let merge (lazy cl) (lazy cr) = lazy (merge_ cl cr)
    
let choice_at a {label_merge} (al,cl) (ar,cr) =
  let sal, sar = lens_get al.lens cl, lens_get ar.lens cr in
  let cl, cr = lens_put al.lens cl Close, lens_put ar.lens cr Close in
  let c = merge cl cr in
  let lr = Send (role sar, label_merge (label sal) (label sar)) in
  lens_put a.lens c lr

let loop c0 = lazy (Lazy.force (Lazy.force c0))


