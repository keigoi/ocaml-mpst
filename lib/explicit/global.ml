open Base
open Session

type ('ka,'kb,'v) channel =
  {sender: 'ka -> 'v -> unit;
   receiver: 'kb -> 'v Lwt.t}

type ('la,'lb,'ca,'cb,'ka,'kb,'v) label =
    {channel: ('ka,'kb,'v) channel;
     select_label: ('v -> 'ca) -> 'la;
     offer_label: 'v * 'cb -> 'lb}

let (-->) : type ra rb ksa ksb sa sb la lb ka kb c0 c1 c2 v.
      (ra, (ksa, sa) prot, (ksa, (rb, ka, la) send) prot, c0, c1) role ->
      (rb, (ksb, sb) prot, (ksb, (ra, kb, lb) receive) prot, c1, c2) role ->
      (la, lb, (ksa,sa) sess, (ksb,sb) sess, ka, kb, v) label ->
      c0 lazy_t -> c2 lazy_t =
  fun a b ({channel;select_label;offer_label}) c0 ->
  let sa = lens_get a.lens c0 in
  let sa =
    Lazy.from_val @@
    Prot (Send (b.role, (fun (k : ka) ks ->
          select_label (fun v -> channel.sender k v; Sess (ks, unprot (Lazy.force sa))))))
  in
  let c1 = lens_put a.lens c0 sa in
  let sb = lens_get b.lens c1 in
  let sb =
    Lazy.from_val @@
    Prot (Receive (a.role, [
          (fun (k : kb) ks ->
            Lwt.map (fun v -> offer_label (v, (Sess (ks, unprot @@ Lazy.force sb)))) (channel.receiver k))]))
  in
  let c2 = lens_put b.lens c1 sb in
  c2


let (-!->) : 'ra 'rb 'ksa 'ksa2 'ksb 'ksb2 'sa 'sb 'la 'lb 'ka 'kb 'c0 'c1 'c2.
      ('ra, ('ksa2, 'sa) prot, ('ksa, ('ksa2, 'rb, 'ka, 'la) request) prot, 'c0, 'c1) role *
        ('ra, unit, 'kb conn, 'ksb, 'ksb2) role ->
      ('rb, ('ksb2, 'sb) prot, ('ksb, ('ksb2, 'ra, 'kb, 'lb) accept) prot, 'c1, 'c2) role *
        ('rb, unit, 'ka conn, 'ksa, 'ksa2) role ->
      ('la, 'lb, ('ksa2,'sa) sess, ('ksb2,'sb) sess, 'ka, 'kb, 'v) label ->
      'c0 lazy_t -> 'c2 lazy_t =
  fun (a,_) (b,_) ({channel;select_label;offer_label}) c0 ->
  let sa = lens_get a.lens c0 in
  let sa =
    Lazy.from_val @@
    Prot (Request (b.role, (fun k ks -> select_label (fun v -> channel.sender k v; Sess (ks, unprot @@ Lazy.force sa)))))
  in
  let c1 = lens_put a.lens c0 sa in
  let sb = lens_get b.lens c1 in
  let sb =
    Lazy.from_val @@
    Prot (Accept (a.role, [(fun k ks -> Lwt.map (fun v -> offer_label (v, (Sess (ks, unprot @@ Lazy.force sb)))) (channel.receiver k))]))
  in
  let c2 = lens_put b.lens c1 sb in
  c2

let (-?->) : type ra rb ksa ksa2 ksb ksb2 sa sb la lb ka kb c0 c1 c2 v.
      (ra, (ksa2, sa) prot, (ksa, (rb, ka, la) send) prot, c0, c1) role *
        (ra, kb conn, unit, ksb, ksb2) role ->
      (rb, (ksb2, sb) prot, (ksb, (ra, kb, lb) receive) prot, c1, c2) role *
        (rb, ka conn, unit, ksa, ksa2) role ->
      (la, lb,
       (ksa, (ksa2, rb, ka, sa) disconnect) sess,
       (ksb, (ksb2, ra, kb, sb) disconnect) sess, ka, kb, v) label ->
      c0 lazy_t -> c2 lazy_t =
  fun (a,_) (b,_) ({channel;select_label;offer_label}) c0 ->
  let sa = lens_get a.lens c0 in
  let sa =
    Lazy.from_val @@
    Prot (Send (b.role, (fun (k : ka) ks ->
          select_label
            (fun v -> channel.sender k v;
                      Sess (ks, Disconnect (b.role, (fun ks' -> Sess (ks', unprot @@ Lazy.force sa))))))))
  in
  let c1 = lens_put a.lens c0 sa in
  let sb = lens_get b.lens c1 in
  let sb =
    Lazy.from_val @@
    Prot (Receive (a.role, [
          (fun (k : kb) ks ->
            Lwt.map (fun v ->
                offer_label (v, (Sess (ks, Disconnect (a.role, (fun ks' -> Sess (ks', unprot @@ Lazy.force sb)))))))
              (channel.receiver k))]))
  in
  let c2 = lens_put b.lens c1 sb in
  c2

let discon :
      ('ra, ('ksa2, 'sa) prot, ('ksa, ('ksa2, 'rb, 'ka, 'sa) disconnect) prot, 'c0, 'c1) role *
        ('ra, 'kb conn, unit, 'ksb, 'ksb2) role ->
      ('rb, ('ksb2, 'sb) prot, ('ksb, ('ksb2, 'ra, 'kb, 'sb) disconnect) prot, 'c1, 'c2) role *
        ('rb, 'ka conn, unit, 'ksa, 'ksa2) role ->
      'c0 lazy_t -> 'c2 lazy_t =
  fun (a,_) (b,_) c0 ->
  let sa = lens_get a.lens c0 in
  let sa =
    Lazy.from_val @@ Prot (Disconnect (b.role, (fun ks -> Sess (ks, unprot @@ Lazy.force sa))))
  in
  let c1 = lens_put a.lens c0 sa in
  let sb = lens_get b.lens c1 in
  let sb =
    Lazy.from_val @@ Prot (Disconnect (a.role, (fun ks -> Sess (ks, unprot @@ Lazy.force sb))))
  in
  let c2 = lens_put b.lens c1 sb in
  c2  

let dummy_receive ra c0 =
  lens_put ra.lens c0 (Lazy.from_val (Prot DummyReceive))

let dummy_close ra c0 =
  lens_put ra.lens c0 (Lazy.from_val (Prot Close))

let dummy_disconnect ra c0 =
  let sa = lens_get_ ra.lens c0 in
  lens_put ra.lens c0 (lv @@ Prot (Disconnect (ra.role, (fun ks -> Sess (ks,unprot sa)))))

type ('l, 'r, 'lr) label_merge =
    {label_merge: 'l -> 'r -> 'lr}

let label : type l ks k r. (ks, (r, k, l) send) prot -> (k -> ks -> l) =
  function
  | Send (_, l) -> l
  | Close -> assert false

let role : type l ks k r. (ks, (r, k, l) send) prot -> r =
  function
  | (Send (r, _)) -> r
  | Close -> assert false

let rec merge_ : type t. t slots lazy_t -> t slots lazy_t -> t slots lazy_t =
  fun l r ->
  match l, r with
  | lazy (Cons(lazy (Prot(hd_l)),tl_l)), lazy (Cons(lazy (Prot(hd_r)),tl_r)) ->
     lazy (Cons (lazy (Prot (Internal.merge hd_l hd_r)), merge_ tl_l tl_r))
  | lazy Nil, _ ->
     Lazy.from_val Nil
  | _ ->
     failwith "merge"

let choice_at a {label_merge} (al,cl) (ar,cr) =
  let sal, sar = lens_get al.lens cl, lens_get ar.lens cr in
  let cl, cr = lens_put al.lens cl (Lazy.from_val (Prot Close)), lens_put ar.lens cr (Lazy.from_val (Prot Close)) in
  let c = merge_ cl cr in
  let lr = lazy (Prot (Send (role (unprot (lf sar)), fun ks k -> label_merge (label (unprot (lf sal)) ks k) (label (unprot (lf sar)) ks k)))) in
  lens_put a.lens c lr

let label : type l ks ks2 k r. (ks, (ks2, r, k, l) request) prot -> (k -> ks2 -> l) =
  function
  | Request (_, l) -> l
  | Close -> assert false

let role : type l ks ks2 k r. (ks, (ks2, r, k, l) request) prot -> r =
  function
  | (Request (r, _)) -> r
  | Close -> assert false

let choice_req_at a {label_merge} (al,cl) (ar,cr) =
  let sal, sar = lens_get al.lens cl, lens_get ar.lens cr in
  let cl, cr = lens_put al.lens cl (lv @@ Prot Close), lens_put ar.lens cr (lv @@ Prot Close) in
  let c = merge_ cl cr in
  let lr = lazy (Prot (Request (role (unprot (lf sar)), fun ks k -> label_merge (label (unprot (lf sal)) ks k) (label (unprot (lf sar)) ks k)))) in
  lens_put a.lens c lr

let loop c0 = lazy (Lazy.force (Lazy.force c0))
