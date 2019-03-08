open Session

type 'v channel =
  {sender: conn -> 'v -> unit;
   receiver: conn -> 'v Lwt.t}

type ('la,'lb,'ca,'cb,'v1,'v2,'v) label =
    {select_label: ('v1 -> 'ca) -> 'la;
     offer_label: 'v2 * 'cb -> 'lb;
     channel:'v channel}


type ('r, 'v1, 'v2, 's1, 's2) role =
  {role:'r;
   lens:('v1, 'v2, 's1, 's2) lens;
   }

let (-->) : type ra rb sa sb la lb c0 c1 c2 v.
      (ra, sa prot, (rb, la) send prot, c0, c1) role ->
      (rb, sb prot, (ra, lb) receive prot, c1, c2) role ->
      (la, lb, sa prot, sb prot, v, v, v) label ->
      c0 lazy_t -> c2 lazy_t =
  fun a b ({select_label;offer_label;channel}) c0 ->
  let sa = lens_get a.lens c0 in
  let sa =
    Lazy.from_val @@
      Send (b.role, 
            fun k -> select_label (fun v -> channel.sender k v; Lazy.force sa))
  in
  let c1 = lens_put a.lens c0 sa in
  let sb = lens_get b.lens c1 in
  let sb =
    Lazy.from_val @@
      Receive (a.role, fun k -> [
            Lwt.map (fun v -> offer_label (v, Lazy.force sb)) (channel.receiver k)])
  in
  let c2 = lens_put b.lens c1 sb in
  c2

(* broadcast *)
let (-->>) : type ra rb sa sb la lb c0 c1 c2 v.
      (ra, sa prot, (rb, la) sendmany prot, c1, c2) role ->
      (rb, sb prot, (ra, lb) receive prot, c0, c1) role ->
      (la, lb, sa prot, sb prot, int -> v, v, v) label ->
      c0 lazy_t -> c2 lazy_t =
  fun a b ({select_label;offer_label;channel}) c0 ->
  let sb = lens_get b.lens c0 in
  let sbs =
    Lazy.from_val @@
        Receive (a.role, fun k -> [
                             channel.receiver k |>
                               Lwt.map (fun v -> offer_label (v, (Lazy.force sb)))
          ])
  in
  let c1 = lens_put b.lens c0 (lazy (Lazy.force sbs)) in
  let sa = lens_get a.lens c1 in
  let sa =
    Lazy.from_val @@
      SendMany (b.role, 
                (fun ks ->
                  select_label (fun f ->
                           List.iteri (fun i k -> channel.sender k (f i)) ks;
                           Lazy.force sa)))
  in
  let c2 = lens_put a.lens c1 sa in
  c2

(* gather *)
let (>>--) : type ra rb sa sb la lb c0 c1 c2 v.
      (ra, sa prot, (rb, la) send prot, c0, c1) role ->
      (rb, sb prot, (ra, lb) receivemany prot, c1, c2) role ->
      (la, lb, sa prot, sb prot, v, v list, v) label ->
      c0 lazy_t -> c2 lazy_t =
  fun a b ({select_label;offer_label;channel}) c0 ->
  let sa = lens_get a.lens c0 in
  let sa =
    Lazy.from_val @@
           Send (b.role, 
                 fun k -> select_label (fun v ->
                              channel.sender k v;
                              Lazy.force sa))
  in
  let c1 = lens_put a.lens c0 sa in
  let sb = lens_get b.lens c1 in
  let sb =
    Lazy.from_val @@
      ReceiveMany (a.role, fun ks -> [
              Lwt_list.map_s (fun k -> channel.receiver k) ks |>
              Lwt.map (fun vs -> offer_label (vs, Lazy.force sb))
        ])
  in
  let c2 = lens_put b.lens c1 sb in
  c2

let dummy_receive ra c0 =
  lens_put ra.lens c0 (Lazy.from_val DummyReceive)

let dummy_close ra c0 =
  lens_put ra.lens c0 (Lazy.from_val Close)

type ('l, 'r, 'lr) label_merge =
    {label_merge: 'l -> 'r -> 'lr}

let label : type l r. (r, l) send prot lazy_t -> conn -> l =
  function
  | lazy (Send (_, l)) -> l
  | lazy Close -> assert false

let labelmany : type l r. (r, l) sendmany prot lazy_t -> conn list -> l =
  function
  | lazy (SendMany (_, l)) -> l
  | lazy Close -> assert false

let role : type l ks k r. (r, l) send prot lazy_t -> r =
  function
  | lazy (Send (r, _)) -> r
  | lazy Close -> assert false

let rolemany : type l ks k r. (r, l) sendmany prot lazy_t -> r =
  function
  | lazy (SendMany (r, _)) -> r
  | lazy Close -> assert false

let rec merge_ : type t. t slots lazy_t -> t slots lazy_t -> t slots lazy_t =
  fun l r ->
  match l, r with
  | lazy (Cons(lazy hd_l,tl_l)), lazy (Cons(lazy hd_r,tl_r)) ->
     lazy (Cons (lazy (Internal.merge hd_l hd_r), merge_ tl_l tl_r))
  | lazy Nil, _ ->
     Lazy.from_val Nil

let choice_at a {label_merge} (al,cl) (ar,cr) =
  let sal, sar = lens_get al.lens cl, lens_get ar.lens cr in
  let cl, cr = lens_put_ al.lens cl Close,
               lens_put_ ar.lens cr Close in
  let c = merge_ cl cr in
  let lr = lazy (Send (role sar, (fun k -> label_merge (label sal k) (label sar k)))) in
  lens_put a.lens c lr

let choicemany_at a {label_merge} (al,cl) (ar,cr) =
  let sal, sar = lens_get al.lens cl, lens_get ar.lens cr in
  let cl, cr = lens_put_ al.lens cl Close,
               lens_put_ ar.lens cr Close in
  let c = merge_ cl cr in
  let lr = lazy (SendMany (rolemany sar, (fun k -> label_merge (labelmany sal k) (labelmany sar k)))) in
  lens_put a.lens c lr

let loop c0 = lazy (Lazy.force (Lazy.force c0))

let get_sess r m =
  lens_get_ r.lens m

let get_sess_many r m =
  lens_get_ r.lens m
