module Make(X:sig type conn end) = struct
  module Session = Session.Make(X)
  open Session

  type 'a one = One__ of 'a
  type 'a many = Many__ of 'a

  type _ e =
    (* slot contents *)
    One : 'a sess lazy_t -> 'a sess one e
  | Many : 'a sess list lazy_t -> 'a sess many e

  let unone : type t. t sess one e -> t sess = function
    One p -> Lazy.force p

  let unsess p =
    match unone p with Sess(_,p) -> p

  let sess p =
    Lazy.from_val @@ Sess(ConnTable.create(),p)

  let sess_ p =
    Sess(ConnTable.create(),p)
           
  let unmany : type t. t sess many e -> t sess list = function
    Many p -> Lazy.force p

  let unsess_many p =
    List.map (fun (Sess(_,p)) -> p) (unmany p) 
     
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
                   (ra, sa sess one, (rb, la) send sess one, c0, c1) role ->
                   (rb, sb sess one, (ra, lb) receive sess one, c1, c2) role ->
                   (la, lb, sa sess, sb sess, v, v) label ->
                   c0 lazy_t -> c2 lazy_t =
    fun a b ({select_label;offer_label;channel}) c0 ->
    let sa = lens_get a.lens c0 in
    let sa =
      Lazy.from_val @@
        One (sess @@
               Send (b.role, 
                     (fun kt -> select_label (fun v -> channel.sender (ConnTable.getone kt b.role) v; Sess(kt,unsess @@ Lazy.force sa)))))
    in
    let c1 = lens_put a.lens c0 sa in
    let sb = lens_get b.lens c1 in
    let sb =
      Lazy.from_val @@
        One (sess @@
               Receive (a.role, [(fun kt ->
                                    Lwt.map (fun v -> offer_label (v, Sess(kt,unsess @@ Lazy.force sb))) (channel.receiver (ConnTable.getone kt a.role)))]))
    in
    let c2 = lens_put b.lens c1 sb in
    c2

  (* broadcast *)
  let (-->>) : type ra rb sa sb la lb c0 c1 c2 v.
                    (ra, sa sess one, (rb, la) sendmany sess one, c1, c2) role ->
                    (rb, sb sess many, (ra, lb) receive sess many, c0, c1) role ->
                    (la, lb, sa sess, sb sess, v, v) label ->
                    c0 lazy_t -> c2 lazy_t =
    fun a b ({select_label;offer_label;channel}) c0 ->
    let sbs = lens_get b.lens c0 in
    let sbs =
      lazy 
        (Many (lazy (List.map
              (fun sb ->                
               sess_ @@
                    Receive (a.role, [(fun kt ->
                             channel.receiver (ConnTable.getone kt a.role) |>
                               Lwt.map (fun v -> offer_label (v, Sess(kt, sb))))])) @@ unsess_many (Lazy.force sbs))))
    in
    let c1 = lens_put b.lens c0 sbs in
    let sa = lens_get a.lens c1 in
    let sa =
      Lazy.from_val @@
        One (sess @@
               SendMany (b.role, 
                         (fun kt k ->
                           select_label (fun v ->
                               channel.sender k v;
                               Sess(kt,unsess @@ Lazy.force sa)))))
    in
    let c2 = lens_put a.lens c1 sa in
    c2

  (* gather *)
  let (>>--) : type ra rb sa sb la lb c0 c1 c2 v.
                    (ra, sa sess many, (rb, la) send sess many, c0, c1) role ->
                    (rb, sb sess one, (ra, lb) receivemany sess one, c1, c2) role ->
                    (la, lb, sa sess, sb sess, v, v list) label ->
                    c0 lazy_t -> c2 lazy_t =
    fun a b ({select_label;offer_label;channel}) c0 ->
    let sas = lens_get a.lens c0 in
    let sa =
      lazy 
        (Many (lazy (List.map
              (fun sa ->                
               sess_ @@
                   Send (b.role, 
                         (fun kt -> select_label (fun v ->
                                        channel.sender (ConnTable.getone kt b.role) v;
                                        Sess(kt, sa)))))
           @@ unsess_many (Lazy.force sas))))
    in
    let c1 = lens_put a.lens c0 sa in
    let sb = lens_get b.lens c1 in
    let sb =
      Lazy.from_val @@
        One (sess @@
                    ReceiveMany (a.role,
                     [(fun kt -> 
                         Lwt_list.map_s (fun k -> channel.receiver k) (ConnTable.getmany kt a.role) |>
                           Lwt.map (fun vs -> offer_label (vs, Sess(kt,unsess @@ Lazy.force sb)))
          )]))
    in
    let c2 = lens_put b.lens c1 sb in
    c2

  let dummy_receive ra c0 =
    lens_put ra.lens c0 (Lazy.from_val (One (sess DummyReceive)))

  let dummy_close ra c0 =
    lens_put ra.lens c0 (Lazy.from_val (One (sess Close)))

  type ('l, 'r, 'lr) label_merge =
    {label_merge: 'l -> 'r -> 'lr}

  let label : type l r. (r, l) send sess one e lazy_t -> ConnTable.t -> l =
    function
    | lazy (One(lazy (Sess(_,Send (_, l))))) -> l
    | lazy (One(lazy (Sess(_,Close)))) -> assert false

  let labelmany : type l r. (r, l) sendmany sess one e lazy_t -> ConnTable.t -> conn -> l =
    function
    | lazy (One(lazy(Sess(_,SendMany (_, l))))) -> l
    | lazy (One(lazy(Sess(_,Close)))) -> assert false

  let role : type l r. (r, l) send sess one e lazy_t -> r =
    function
    | lazy (One(lazy(Sess(_,Send (r, _))))) -> r
    | lazy (One(lazy(Sess(_,Close)))) -> assert false

  let rolemany : type l r. (r, l) sendmany sess one e lazy_t -> r =
    function
    | lazy (One(lazy(Sess(_,SendMany (r, _))))) -> r
    | lazy (One(lazy(Sess(_,Close)))) -> assert false

  let rec merge_ : type t. t slots lazy_t -> t slots lazy_t -> t slots lazy_t =
    fun l r ->
    match l, r with
    | lazy (Cons(lazy (One(lazy(Sess(_,hd_l)))),tl_l)), lazy (Cons(lazy (One(lazy(Sess(_,hd_r)))),tl_r)) ->
       lazy (Cons (lazy (One(sess @@ Internal.merge hd_l hd_r)), merge_ tl_l tl_r))
    | lazy (Cons(lazy (Many (lazy hd_l)),tl_l)), lazy (Cons(lazy (Many (lazy hd_r)),tl_r)) ->
       lazy (Cons (lazy (Many (lazy (List.rev @@ List.rev_map2 (fun (Sess(_,x)) (Sess(_,y)) -> sess_ @@ Internal.merge x y) hd_l hd_r))), merge_ tl_l tl_r))
    | lazy Nil, _ ->
       Lazy.from_val Nil

  let choice_at a {label_merge} (al,cl) (ar,cr) =
    let sal, sar = lens_get al.lens cl, lens_get ar.lens cr in
    let cl, cr = lens_put_ al.lens cl (One(sess Close)),
                 lens_put_ ar.lens cr (One(sess Close)) in
    let c = merge_ cl cr in
    let lr = One (sess @@ Send (role sar, (fun k -> label_merge (label sal k) (label sar k)))) in
    lens_put_ a.lens c lr

  let choicemany_at a {label_merge} (al,cl) (ar,cr) =
    let sal, sar = lens_get al.lens cl, lens_get ar.lens cr in
    let cl, cr = lens_put_ al.lens cl (One(sess Close)),
                 lens_put_ ar.lens cr (One(sess Close)) in
    let c = merge_ cl cr in
    let lr = One(sess @@ SendMany (rolemany sar, (fun ks k -> label_merge (labelmany sal ks k) (labelmany sar ks k)))) in
    lens_put_ a.lens c lr

  let loop c0 = lazy (Lazy.force (Lazy.force c0))

  (* let count r cnt c0 =
   *   let Prot(p) = lens_get_ r.lens c0 in
   *   lens_put_ r.lens c0 (ProtCount(p,cnt))
   *             
   * let get_sess r m =
   *   Sess(ConnTable.create (), unprot @@ lens_get_ r.lens m)
   * 
   * let add_conn r k (Sess(kt,p)) = Sess(ConnTable.putone kt r k, p)
   * 
   * let add_conn_many r ks (Sess(kt,p)) = Sess(ConnTable.putmany kt r ks, p) *)
  let get_sess r m =
    unone @@ lens_get_ r.lens m
    
  let get_sess_many r m =
    unmany @@ lens_get_ r.lens m
    
  let nil = Lazy.from_val Nil
  let one tl = Lazy.from_val @@ Cons(Lazy.from_val @@ One(sess Close), tl)

  open Mpst_base

  let many_at (role: ('r, close sess one, close sess many, 'c0, 'c1) role) num (cont:'c0 lazy_t) =
    let cs =
      repeat num (fun i -> sess_ Close)
    in
    lens_put role.lens cont (lazy (Many (lazy cs)))
    

  let many_at_ role num cont =
    let cs =
      repeat num (fun i ->
          List.nth (unmany @@ lens_get_ role.lens cont) i)
    in
    lens_put role.lens cont (lazy (Many (lazy cs)))
end
