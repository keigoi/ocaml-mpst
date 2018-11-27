open Base
open Session

type ('ka,'kb,'v) channel =
  {sender: 'ka conn -> 'v -> unit;
   receiver: 'kb conn -> 'v Lwt.t}

type ('la,'lb,'ca,'cb,'ka,'kb,'v) label =
    {make_channel: unit -> ('ka,'kb,'v) channel;
     select_label: ('v -> 'ca) -> 'la;
     offer_label: 'v * 'cb -> 'lb}

let (-->) : 'ra 'rb 'ksa 'ksb 'sa 'sb 'la 'lb 'ka 'kb 'c0 'c1 'c2.
      ('ra, ('ksa, 'sa) prot, ('ksa, ('rb, 'ka, 'la) send) prot, 'c0, 'c1) role ->
      ('rb, ('ksb, 'sb) prot, ('ksb, ('ra, 'kb, 'lb) receive) prot, 'c1, 'c2) role ->
      ('la, 'lb, ('ksa,'sa) sess, ('ksb,'sb) sess, 'ka, 'kb, 'v) label ->
      'c0 lazy_t -> 'c2 lazy_t =
  fun a b ({make_channel;select_label;offer_label}) c0 ->
  let c = make_channel () in
  let sa = lazy (a.lens.get c0) in
  let sa =
    Send (b.role, (fun k ks -> select_label (fun v -> c.sender k v; Sess (ks, Lazy.force sa))))
  in
  let c1 = a.lens.put c0 sa in
  let sb = lazy (b.lens.get c1) in
  let sb =
    Receive (a.role, [(fun k ks -> Lwt.map (fun v -> offer_label (v, (Sess (ks, Lazy.force sb)))) (c.receiver k))])
  in
  let c2 = b.lens.put c1 sb in
  c2


let (-!->) : 'ra 'rb 'ksa 'ksa2 'ksb 'ksb2 'sa 'sb 'la 'lb 'ka 'kb 'c0 'c1 'c2.
      ('ra, ('ksa2, 'sa) prot, ('ksa, ('ksa2, 'rb, 'ka, 'la) request) prot, 'c0, 'c1) role *
        ('ra, unit, 'kb conn, 'ksb, 'ksb2) role ->
      ('rb, ('ksb2, 'sb) prot, ('ksb, ('ksb2, 'ra, 'kb, 'lb) accept) prot, 'c1, 'c2) role *
        ('rb, unit, 'ka conn, 'ksa, 'ksa2) role ->
      ('la, 'lb, ('ksa2,'sa) sess, ('ksb2,'sb) sess, 'ka, 'kb, 'v) label ->
      'c0 lazy_t -> 'c2 lazy_t =
  fun (a,_) (b,_) ({make_channel;select_label;offer_label}) c0 ->
  let c = make_channel () in
  let sa = lazy (a.lens.get c0) in
  let sa =
    Request (b.role, (fun k ks -> select_label (fun v -> c.sender k v; Sess (ks, Lazy.force sa))))
  in
  let c1 = a.lens.put c0 sa in
  let sb = lazy (b.lens.get c1) in
  let sb =
    Accept (a.role, [(fun k ks -> Lwt.map (fun v -> offer_label (v, (Sess (ks, Lazy.force sb)))) (c.receiver k))])
  in
  let c2 = b.lens.put c1 sb in
  c2

let discon :
      ('ra, ('ksa2, 'sa) prot, ('ksa, ('ksa2, 'rb, 'ka, 'sa) disconnect) prot, 'c0, 'c1) role ->
      ('rb, ('ksb2, 'sb) prot, ('ksb, ('ksb2, 'ra, 'kb, 'sb) disconnect) prot, 'c1, 'c2) role ->
      'c0 lazy_t -> 'c2 lazy_t =
  fun a b c0 ->
  let sa = lazy (a.lens.get c0) in
  let sa =
    Disconnect (b.role, (fun ks -> Sess (ks, Lazy.force sa)))
  in
  let c1 = a.lens.put c0 sa in
  let sb = lazy (b.lens.get c1) in
  let sb =
    Disconnect (a.role, (fun ks -> Sess (ks, Lazy.force sb)))
  in
  let c2 = b.lens.put c1 sb in
  c2

let dummy_receive ra c0 =
  ra.lens.put c0 DummyReceive

let dummy_close ra c0 =
  ra.lens.put c0 Close

type ('l, 'r, 'lr) label_merge =
    {label_merge: 'l -> 'r -> 'lr}

let label : type l ks k r. (ks, (r, k, l) send) prot -> (k conn -> ks -> l) =
  fun (Send (_, l)) -> l

let role : type l ks k r. (ks, (r, k, l) send) prot -> r =
  fun (Send (r, _)) -> r

let choice_at merge a {label_merge} (al,cl) (ar,cr) =
  let sal, sar = al.lens.get cl, ar.lens.get cr in
  let cl, cr = al.lens.put cl Close, ar.lens.put cr Close in
  let c = merge cl cr in
  let lr = Send (role sar, fun ks k -> label_merge (label sal ks k) (label sar ks k)) in
  a.lens.put c lr

let loop c0 = lazy (Lazy.force (Lazy.force c0))
