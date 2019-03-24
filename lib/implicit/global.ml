open Mpst_base

let (>>=) = Lwt.(>>=)

module Make(Flag:S.FLAG)(X:sig type conn end) = struct
  module Session = Session.Make(Flag)(X)
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
    match unone p with {prot; _} -> prot

  let sess_ p =
    {once=Flag.create();
     conn=ConnTable.create();
     prot=p}

  let sess p =
    Lazy.from_val @@ sess_ p
           
  let unmany : type t. t sess many e -> t sess list = function
    Many p -> Lazy.force p

  let unsess_many p =
    List.map (fun {prot; _} -> prot) (unmany p) 
     
  include LensLazy.Make(struct type 't u = 't e end)

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
                     (fun kt ->
                       select_label (fun v ->
                           channel.sender (ConnTable.getone kt b.role) v;
                           {once=Flag.create();
                            conn=kt;
                            prot=unsess @@ Lazy.force sa}))))
    in
    let c1 = lens_put a.lens c0 sa in
    let sb = lens_get b.lens c1 in
    let sb =
      Lazy.from_val @@
        One (sess @@
               Receive (a.role,
                        [(fun kt ->
                            channel.receiver (ConnTable.getone kt a.role) |> Lwt.map @@ fun v ->
                            offer_label
                              (v, {once=Flag.create(); conn=kt; prot=unsess @@ Lazy.force sb}))]))
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
      lazy begin
          unsess_many (Lazy.force sbs) |>
          List.map (fun sb ->
                sess_ @@
                  Receive (a.role,
                           [(fun kt ->
                             channel.receiver (ConnTable.getone kt a.role) |> Lwt.map @@ fun v ->
                             offer_label (v, {once=Flag.create();conn=kt;prot=sb}))]))
        end
    in
    let c1 = lens_put b.lens c0 (Lazy.from_val @@ Many sbs) in
    let sa = lens_get a.lens c1 in
    let sa =
      Lazy.from_val @@
        One (sess @@
               SendMany (b.role, 
                         (fun kt k ->
                           select_label (fun v ->
                               channel.sender k v;
                               {once=Flag.create();
                                conn=kt;
                                prot=unsess @@ Lazy.force sa}))))
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
    let sas =
      lazy begin
          unsess_many (Lazy.force sas) |>
          List.map (fun sa ->                
               sess_ @@ Send (b.role, 
                              (fun kt ->
                                select_label (fun v ->
                                    channel.sender (ConnTable.getone kt b.role) v;
                                    {once=Flag.create();
                                     conn=kt;
                                     prot=sa}))))
        end
    in
    let c1 = lens_put a.lens c0 (Lazy.from_val @@ Many sas) in
    let sb = lens_get b.lens c1 in
    let sb =
      Lazy.from_val @@
        One (sess @@ ReceiveMany
                       (a.role,
                        [(fun kt ->
                            let ks = ConnTable.getmany kt a.role in
                            Lwt_list.map_s (fun k -> channel.receiver k) ks |> Lwt.map @@ fun vs ->
                            offer_label (vs, {once=Flag.create(); conn=kt; prot=unsess @@ Lazy.force sb})
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
    | lazy (One(lazy {prot=Send (_, l); _})) -> l
    | lazy (One(lazy {prot=Close; _})) -> assert false

  let labelmany : type l r. (r, l) sendmany sess one e lazy_t -> ConnTable.t -> conn -> l =
    function
    | lazy (One(lazy {prot=SendMany(_, l); _})) -> l
    | lazy (One(lazy {prot=Close; _})) -> assert false

  let role : type l r. (r, l) send sess one e lazy_t -> r =
    function
    | lazy (One(lazy {prot=Send(r, _); _})) -> r
    | lazy (One(lazy {prot=Close; _})) -> assert false

  let rolemany : type l r. (r, l) sendmany sess one e lazy_t -> r =
    function
    | lazy (One(lazy {prot=SendMany(r, _); _})) -> r
    | lazy (One(lazy {prot=Close; _})) -> assert false

  let rec merge_ : type t. t slots lazy_t -> t slots lazy_t -> t slots lazy_t =
    fun l r ->
    match l, r with
    | lazy (Cons(lazy (One(lazy {prot=hd_l;_})),tl_l)), lazy (Cons(lazy (One(lazy {prot=hd_r;_})),tl_r)) ->
       lazy (Cons (lazy (One(sess @@ Internal.merge hd_l hd_r)), merge_ tl_l tl_r))
    | lazy (Cons(lazy (Many (lazy hd_l)),tl_l)), lazy (Cons(lazy (Many (lazy hd_r)),tl_r)) ->
       lazy (Cons (lazy (Many (lazy (List.rev @@
                                       List.rev_map2
                                         (fun {prot=x; _} {prot=y; _} ->
                                           sess_ @@ Internal.merge x y) hd_l hd_r))),
                   merge_ tl_l tl_r))
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
