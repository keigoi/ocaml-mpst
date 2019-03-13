module Make(X:sig type conn end) = struct
  module Session = Session.Make(X)
  open Session

  type 't one = 't

  type 't count = Count__

  type _ e =
    Prot : 't prot -> 't prot e
  | ProtCount : 't prot * int -> 't prot count e
  | Session : 't sess list -> 't sess e
           
  let unprot : type t. t prot e -> t prot = function
    Prot p -> p
           
  let unprotcount : type t. t prot count e -> t prot * int = function
    ProtCount(p,i) -> p, i
     
  include Mpst_base.Lens.Make(struct type 't u = 't e end)

  type 'v channel =
    {sender: conn -> 'v -> unit;
     receiver: conn -> 'v Lwt.t}

  type ('la,'lb,'ca,'cb,'v1, 'v2) label =
    {select_label: ('v1 -> 'ca) -> 'la;
     offer_label: 'v2 * 'cb -> 'lb;
     channel:'v1 channel}


  type ('r, 'v1, 'v2, 's1, 's2) role =
    {role:'r;
     lens:('v1, 'v2, 's1, 's2) lens;
    }

  let (-->) : type ra rb sa sb la lb c0 c1 c2 v.
                   (ra, sa prot, (rb, la) send prot, c0, c1) role ->
                   (rb, sb prot, (ra, lb) receive prot, c1, c2) role ->
                   (la, lb, sa sess, sb sess, v, v) label ->
                   c0 lazy_t -> c2 lazy_t =
    fun a b ({select_label;offer_label;channel}) c0 ->
    let sa = lens_get a.lens c0 in
    let sa =
      Lazy.from_val @@
        Prot (Send (b.role, 
              fun kt -> select_label (fun v -> channel.sender (ConnTable.getone kt b.role) v; Sess(kt,unprot @@ Lazy.force sa))))
    in
    let c1 = lens_put a.lens c0 sa in
    let sb = lens_get b.lens c1 in
    let sb =
      Lazy.from_val @@
        Prot (Receive (a.role, [(fun kt ->
                             Lwt.map (fun v -> offer_label (v, Sess(kt,unprot @@ Lazy.force sb))) (channel.receiver (ConnTable.getone kt a.role)))]))
    in
    let c2 = lens_put b.lens c1 sb in
    c2

  (* broadcast *)
  let (-->>) : type ra rb sa sb la lb c0 c1 c2 v.
                    (ra, sa prot, (rb, la) sendmany prot, c1, c2) role ->
                    (rb, sb prot, (ra, lb) receive prot, c0, c1) role ->
                    (la, lb, sa sess, sb sess, v, v) label ->
                    c0 lazy_t -> c2 lazy_t =
    fun a b ({select_label;offer_label;channel}) c0 ->
    let sb = lens_get b.lens c0 in
    let sbs =
      Lazy.from_val @@
        Prot (Receive (a.role, [(fun kt ->
                             channel.receiver (ConnTable.getone kt a.role) |>
                               Lwt.map (fun v -> offer_label (v, Sess(kt,unprot @@ Lazy.force sb)))
          )]))
    in
    let c1 = lens_put b.lens c0 sbs in
    let sa = lens_get a.lens c1 in
    let sa =
      Lazy.from_val @@
        Prot (SendMany (b.role, 
                  (fun kt k ->
                    select_label (fun v ->
                        channel.sender k v;
                        Sess(kt,unprot @@ Lazy.force sa)))))
    in
    let c2 = lens_put a.lens c1 sa in
    c2

  (* gather *)
  let (>>--) : type ra rb sa sb la lb c0 c1 c2 v.
                    (ra, sa prot, (rb, la) send prot, c0, c1) role ->
                    (rb, sb prot, (ra, lb) receivemany prot, c1, c2) role ->
                    (la, lb, sa sess, sb sess, v, v list) label ->
                    c0 lazy_t -> c2 lazy_t =
    fun a b ({select_label;offer_label;channel}) c0 ->
    let sa = lens_get a.lens c0 in
    let sa =
      Lazy.from_val @@
        Prot (Send (b.role, 
              fun kt -> select_label (fun v ->
                           channel.sender (ConnTable.getone kt b.role) v;
                           Sess(kt, unprot @@ Lazy.force sa))))
    in
    let c1 = lens_put a.lens c0 sa in
    let sb = lens_get b.lens c1 in
    let sb =
      Lazy.from_val @@
        Prot (ReceiveMany (a.role,
                     [(fun kt -> 
                         Lwt_list.map_s (fun k -> channel.receiver k) (ConnTable.getmany kt a.role) |>
                           Lwt.map (fun vs -> offer_label (vs, Sess(kt,unprot @@ Lazy.force sb)))
          )]))
    in
    let c2 = lens_put b.lens c1 sb in
    c2

  let dummy_receive ra c0 =
    lens_put ra.lens c0 (Lazy.from_val (Prot DummyReceive))

  let dummy_close ra c0 =
    lens_put ra.lens c0 (Lazy.from_val (Prot Close))

  type ('l, 'r, 'lr) label_merge =
    {label_merge: 'l -> 'r -> 'lr}

  let label : type l r. (r, l) send prot e lazy_t -> ConnTable.t -> l =
    function
    | lazy (Prot(Send (_, l))) -> l
    | lazy (Prot Close) -> assert false

  let labelmany : type l r. (r, l) sendmany prot e lazy_t -> ConnTable.t -> conn -> l =
    function
    | lazy (Prot(SendMany (_, l))) -> l
    | lazy (Prot Close) -> assert false

  let role : type l r. (r, l) send prot e lazy_t -> r =
    function
    | lazy (Prot(Send (r, _))) -> r
    | lazy (Prot Close) -> assert false

  let rolemany : type l r. (r, l) sendmany prot e lazy_t -> r =
    function
    | lazy (Prot (SendMany (r, _))) -> r
    | lazy (Prot Close) -> assert false

  let rec merge_ : type t. t slots lazy_t -> t slots lazy_t -> t slots lazy_t =
    fun l r ->
    match l, r with
    | lazy (Cons(lazy (Prot hd_l),tl_l)), lazy (Cons(lazy (Prot hd_r),tl_r)) ->
       lazy (Cons (lazy (Prot(Internal.merge hd_l hd_r)), merge_ tl_l tl_r))
    | lazy Nil, _ ->
       Lazy.from_val Nil
    | _ -> failwith "merge_"

  let choice_at a {label_merge} (al,cl) (ar,cr) =
    let sal, sar = lens_get al.lens cl, lens_get ar.lens cr in
    let cl, cr = lens_put_ al.lens cl (Prot(Close)),
                 lens_put_ ar.lens cr (Prot(Close)) in
    let c = merge_ cl cr in
    let lr = lazy (Prot (Send (role sar, (fun k -> label_merge (label sal k) (label sar k))))) in
    lens_put a.lens c lr

  let choicemany_at a {label_merge} (al,cl) (ar,cr) =
    let sal, sar = lens_get al.lens cl, lens_get ar.lens cr in
    let cl, cr = lens_put_ al.lens cl (Prot Close),
                 lens_put_ ar.lens cr (Prot Close) in
    let c = merge_ cl cr in
    let lr = lazy (Prot(SendMany (rolemany sar, (fun ks k -> label_merge (labelmany sal ks k) (labelmany sar ks k))))) in
    lens_put a.lens c lr

  let loop c0 = lazy (Lazy.force (Lazy.force c0))

  let count r cnt c0 =
    let Prot(p) = lens_get_ r.lens c0 in
    lens_put_ r.lens c0 (ProtCount(p,cnt))
              
  let get_sess r m =
    Sess(ConnTable.create (), unprot @@ lens_get_ r.lens m)

  let add_conn r k (Sess(kt,p)) = Sess(ConnTable.putone kt r k, p)

  let add_conn_many r ks (Sess(kt,p)) = Sess(ConnTable.putmany kt r ks, p)
    
  let nil = Lazy.from_val Nil
  let one tl = Lazy.from_val @@ Cons(Lazy.from_val @@ Prot Close, tl)
end
