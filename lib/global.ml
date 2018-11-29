open Base
open Session

type ('ka,'kb,'v) channel =
  {sender: 'ka conn -> 'v -> unit;
   receiver: 'kb conn -> 'v Lwt.t}

type ('la,'lb,'ca,'cb,'ka,'kb,'v) label =
    {channel: ('ka,'kb,'v) channel;
     select_label: ('v -> 'ca) -> 'la;
     offer_label: 'v * 'cb -> 'lb}

let make_shmem_channel () =
  let st, push = Lwt_stream.create () in
  {receiver=(fun Memory -> Lwt_stream.next st);
   sender=(fun Memory v -> push (Some v))}
  
let (-->) : type ra rb ksa ksb sa sb la lb ka kb c0 c1 c2 v.
      (ra, (ksa, sa) prot, (ksa, (rb, ka, la) send) prot, c0, c1) role ->
      (rb, (ksb, sb) prot, (ksb, (ra, kb, lb) receive) prot, c1, c2) role ->
      (la, lb, (ksa,sa) sess, (ksb,sb) sess, ka, kb, v) label ->
      c0 lazy_t -> c2 lazy_t =
  fun a b ({channel;select_label;offer_label}) c0 ->
  let mch = make_shmem_channel () in
  let sa = lazy (a.lens.get c0) in
  let sa =
    Send (b.role, (fun (k : ka conn) ks ->
          let sender : v -> unit  =
            match k with
            | Memory -> mch.sender Memory
            | Conn _  -> channel.sender k
          in
          select_label (fun v -> sender v; Sess (ks, Lazy.force sa))))
  in
  let c1 = a.lens.put c0 sa in
  let sb = lazy (b.lens.get c1) in
  let sb =
    Receive (a.role, [
          (fun (k : kb conn) ks ->
            let receiver : v Lwt.t =
              match k with
              | Memory -> mch.receiver Memory
              | Conn _ -> channel.receiver k
            in
            Lwt.map (fun v -> offer_label (v, (Sess (ks, Lazy.force sb)))) receiver)])
  in
  let c2 = b.lens.put c1 sb in
  c2


let (-!->) : 'ra 'rb 'ksa 'ksa2 'ksb 'ksb2 'sa 'sb 'la 'lb 'ka 'kb 'c0 'c1 'c2.
      ('ra, ('ksa2, 'sa) prot, ('ksa, ('ksa2, 'rb, 'ka dist, 'la) request) prot, 'c0, 'c1) role *
        ('ra, unit, 'kb dist conn, 'ksb, 'ksb2) role ->
      ('rb, ('ksb2, 'sb) prot, ('ksb, ('ksb2, 'ra, 'kb dist, 'lb) accept) prot, 'c1, 'c2) role *
        ('rb, unit, 'ka dist conn, 'ksa, 'ksa2) role ->
      ('la, 'lb, ('ksa2,'sa) sess, ('ksb2,'sb) sess, 'ka dist, 'kb dist, 'v) label ->
      'c0 lazy_t -> 'c2 lazy_t =
  fun (a,_) (b,_) ({channel;select_label;offer_label}) c0 ->
  let sa = lazy (a.lens.get c0) in
  let sa =
    Request (b.role, (fun k ks -> select_label (fun v -> channel.sender k v; Sess (ks, Lazy.force sa))))
  in
  let c1 = a.lens.put c0 sa in
  let sb = lazy (b.lens.get c1) in
  let sb =
    Accept (a.role, [(fun k ks -> Lwt.map (fun v -> offer_label (v, (Sess (ks, Lazy.force sb)))) (channel.receiver k))])
  in
  let c2 = b.lens.put c1 sb in
  c2

let (-?->) : type ra rb ksa ksa2 ksb ksb2 sa sb la lb ka kb c0 c1 c2 v.
      (ra, (ksa2, sa) prot, (ksa, (rb, ka dist, la) send) prot, c0, c1) role *
        (ra, kb dist conn, unit, ksb, ksb2) role ->
      (rb, (ksb2, sb) prot, (ksb, (ra, kb dist, lb) receive) prot, c1, c2) role *
        (rb, ka dist conn, unit, ksa, ksa2) role ->
      (la, lb,
       (ksa, (ksa2, rb, ka dist, sa) disconnect) sess,
       (ksb, (ksb2, ra, kb dist, sb) disconnect) sess, ka dist, kb dist, v) label ->
      c0 lazy_t -> c2 lazy_t =
  fun (a,_) (b,_) ({channel;select_label;offer_label}) c0 ->
  let sa = lazy (a.lens.get c0) in
  let sa =
    Send (b.role, (fun (k : ka dist conn) ks ->
          let sender : v -> unit  =
            match k with
            | Conn _  -> channel.sender k
          in
          select_label
            (fun v -> sender v;
                      Sess (ks, Disconnect (b.role, (fun ks' -> Sess (ks', Lazy.force sa)))))))
  in
  let c1 = a.lens.put c0 sa in
  let sb = lazy (b.lens.get c1) in
  let sb =
    Receive (a.role, [
          (fun (k : kb dist conn) ks ->
            let receiver : v Lwt.t =
              match k with
              | Conn _ -> channel.receiver k
            in
            Lwt.map (fun v ->
                offer_label (v, (Sess (ks, Disconnect (a.role, (fun ks' -> Sess (ks', Lazy.force sb)))))))
              receiver)])
  in
  let c2 = b.lens.put c1 sb in
  c2

let discon :
      ('ra, ('ksa2, 'sa) prot, ('ksa, ('ksa2, 'rb, 'ka dist, 'sa) disconnect) prot, 'c0, 'c1) role *
        ('ra, 'kb dist conn, unit, 'ksb, 'ksb2) role ->
      ('rb, ('ksb2, 'sb) prot, ('ksb, ('ksb2, 'ra, 'kb dist, 'sb) disconnect) prot, 'c1, 'c2) role *
        ('rb, 'ka dist conn, unit, 'ksa, 'ksa2) role ->
      'c0 lazy_t -> 'c2 lazy_t =
  fun (a,_) (b,_) c0 ->
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

let dummy_disconnect ra c0 =
  let sa = ra.lens.get c0 in
  ra.lens.put c0 (Disconnect (ra.role, (fun ks -> Sess (ks,sa))))

type ('l, 'r, 'lr) label_merge =
    {label_merge: 'l -> 'r -> 'lr}

let label : type l ks k r. (ks, (r, k, l) send) prot -> (k conn -> ks -> l) =
  function
  | Send (_, l) -> l
  | Close -> assert false

let role : type l ks k r. (ks, (r, k, l) send) prot -> r =
  function
  | (Send (r, _)) -> r
  | Close -> assert false

let choice_at merge a {label_merge} (al,cl) (ar,cr) =
  let sal, sar = al.lens.get cl, ar.lens.get cr in
  let cl, cr = al.lens.put cl Close, ar.lens.put cr Close in
  let c = merge cl cr in
  let lr = Send (role sar, fun ks k -> label_merge (label sal ks k) (label sar ks k)) in
  a.lens.put c lr

let label : type l ks ks2 k r. (ks, (ks2, r, k, l) request) prot -> (k conn -> ks2 -> l) =
  function
  | Request (_, l) -> l
  | Close -> assert false

let role : type l ks ks2 k r. (ks, (ks2, r, k, l) request) prot -> r =
  function
  | (Request (r, _)) -> r
  | Close -> assert false

let choice_req_at merge a {label_merge} (al,cl) (ar,cr) =
  let sal, sar = al.lens.get cl, ar.lens.get cr in
  let cl, cr = al.lens.put cl Close, ar.lens.put cr Close in
  let c = merge cl cr in
  let lr = Request (role sar, fun ks k -> label_merge (label sal ks k) (label sar ks k)) in
  a.lens.put c lr

let loop c0 = lazy (Lazy.force (Lazy.force c0))
