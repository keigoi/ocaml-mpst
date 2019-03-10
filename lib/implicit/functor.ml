
module Make(X:sig type conn end) = struct
  module Session = struct
    type ('r,'ls) send = DummySend__
    type ('r,'ls) sendmany = DummySendMany__
    type ('r,'ls) receive = DummyReceive__
    type ('r,'ls) receivemany = DummyReceiveMany__
    type close

    exception RoleNotEnabled
    exception ReceiveFail
            
    type conn = X.conn
    type 't one = One__ of 't
    type 't many = Many__ of 't
                           
                           
    type _ ep =
      EPOne : conn * 'r -> 'r one ep
    | EPMany : conn list * 'r -> 'r many ep
    
    type _ slots =
      Cons : 'x prot lazy_t * 'xs slots lazy_t -> ('x prot * 'xs) slots
    | Nil : unit slots

    and (_,_,_,_) lens =
      | Fst  : ('a prot, 'b prot, ('a prot * 'xs) slots, ('b prot * 'xs) slots) lens
      | Next : ('a,'b, 'xs slots,'ys slots) lens
               -> ('a,'b, ('x * 'xs) slots, ('x * 'ys) slots) lens

    and _ prot =
      | Send :
          'r * (conn -> 'ls)
          -> ('r, 'ls) send prot
      | SendMany :
          'r * (conn -> 'ls)
          -> ('r, 'ls) sendmany prot
      | Receive :
          'r * (conn -> 'ls Lwt.t) list
          -> ('r, 'ls) receive prot
      | ReceiveMany :
          'r * ((conn list -> 'ls Lwt.t) list)
          -> ('r, 'ls) receivemany prot
      | Close : close prot
      | DummyReceive :
          ('r, 'ls) receive prot

    module ConnTable : sig
      type t
      val create : unit -> t
      val get : t -> 'k -> conn
      val put : t -> 'k -> conn -> unit
    end = struct
      type t = (Obj.t, conn) Hashtbl.t
      let create () = Hashtbl.create 42
      let get t k = Hashtbl.find t (Obj.repr k)
      let put t k v = Hashtbl.add t (Obj.repr k) v
    end

    let slot_tail : type hd tl. (hd * tl) slots lazy_t -> tl slots lazy_t = fun sl ->
      lazy begin
          match sl with
          | lazy (Cons(_,lazy tl)) -> tl
        end
      
    let slot_head : type hd tl. (hd * tl) slots lazy_t -> hd lazy_t = fun sl ->
      lazy begin
          match sl with
          | lazy (Cons(lazy hd,_)) -> hd
        end
      
    let rec lens_get : type a b xs ys. (a, b, xs, ys) lens -> xs lazy_t -> a lazy_t = fun ln xs ->
      match ln with
      | Fst -> slot_head xs
      | Next ln' -> lens_get ln' (slot_tail xs)

    let lens_get_ ln s = Lazy.force (lens_get ln s)

    let rec lens_put : type a b xs ys. (a,b,xs,ys) lens -> xs lazy_t -> b lazy_t -> ys lazy_t =
      fun ln xs b ->
      match ln with
      | Fst -> lazy (Cons(b, slot_tail xs))
      | Next ln' ->
         lazy
           begin match xs with
           | lazy (Cons(a, xs')) -> Cons(a, lens_put ln' xs' b)
           end

    let lens_put_ ln s v = lens_put ln s (Lazy.from_val v)

    let send : 'r 'ls 'v 's.
               'r one ep ->
               ((< .. > as 'ls) -> 'v -> 's prot) ->
               'v ->
               ('r, 'ls) send prot ->
               's prot =
      fun (EPOne(k,_)) sel v (Send (_,f)) ->
      let s = sel (f k) v in
      s

    let multicast : 'r 'ls 'v 's.
                    'r many ep ->
                    ((< .. > as 'ls) -> 'v -> 's prot) ->
                    (int -> 'v) ->
                    ('r, 'ls) sendmany prot ->
                    's prot =
      fun (EPMany(ks,_)) sel f (SendMany (_,ls)) ->
      match List.mapi (fun i k -> sel (ls k) (f i)) ks with
      | [] -> failwith "no connection"
      | s::_ -> s

    let rec first k = function
      | [] -> Lwt.fail (Failure "receive failed")
      | f::fs ->
         Lwt.catch (fun () -> f k) (function
         | ReceiveFail -> first k fs
         | e -> Lwt.fail e)
              
    let receive : 'r 'ls.
                  'r one ep ->
                  ('r, 'ls) receive prot -> 'ls Lwt.t =
      fun (EPOne(k,_)) s ->
      match s with
      | Receive(_, fs) ->
         first k fs
      | DummyReceive ->
         failwith "DummyReceive encountered" 

    let gather : 'r 'ls.
                 'r many ep ->
                 ('r, 'ls) receivemany prot -> 'ls Lwt.t =
      fun (EPMany(ks,_)) s ->
      match s with
      | ReceiveMany(_, f) ->
         first ks f

    let close Close = ()

    module Internal = struct
      
      let merge : type t. t prot -> t prot -> t prot = fun x y ->
        match x, y with
        | Send _, Send _ ->
           raise RoleNotEnabled
        | SendMany _, SendMany _ ->
           raise RoleNotEnabled
        | Receive (r, xs), Receive (_, ys) ->
           Receive (r, xs @ ys)
        | ReceiveMany (r, xs), ReceiveMany (_, ys) ->
           ReceiveMany (r, xs @ ys)
        | Receive (r, xs), DummyReceive ->
           Receive (r, xs)
        | DummyReceive, Receive (r, xs) ->
           Receive (r, xs)
        | DummyReceive, DummyReceive ->
           DummyReceive
        | Close, Close ->
           Close
    end
  end

  module Global = struct
    open Session

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
                     (la, lb, sa prot, sb prot, v, v) label ->
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
          Receive (a.role, [(fun k ->
                               Lwt.map (fun v -> offer_label (v, Lazy.force sb)) (channel.receiver k))])
      in
      let c2 = lens_put b.lens c1 sb in
      c2

    (* broadcast *)
    let (-->>) : type ra rb sa sb la lb c0 c1 c2 v.
                      (ra, sa prot, (rb, la) sendmany prot, c1, c2) role ->
                      (rb, sb prot, (ra, lb) receive prot, c0, c1) role ->
                      (la, lb, sa prot, sb prot, v, v) label ->
                      c0 lazy_t -> c2 lazy_t =
      fun a b ({select_label;offer_label;channel}) c0 ->
      let sb = lens_get b.lens c0 in
      let sbs =
        Lazy.from_val @@
          Receive (a.role, [(fun k ->
                               channel.receiver k |>
                                 Lwt.map (fun v -> offer_label (v, (Lazy.force sb)))
            )])
      in
      let c1 = lens_put b.lens c0 (lazy (Lazy.force sbs)) in
      let sa = lens_get a.lens c1 in
      let sa =
        Lazy.from_val @@
          SendMany (b.role, 
                    (fun k ->
                      select_label (fun v ->
                          channel.sender k v;
                          Lazy.force sa)))
      in
      let c2 = lens_put a.lens c1 sa in
      c2

    (* gather *)
    let (>>--) : type ra rb sa sb la lb c0 c1 c2 v.
                      (ra, sa prot, (rb, la) send prot, c0, c1) role ->
                      (rb, sb prot, (ra, lb) receivemany prot, c1, c2) role ->
                      (la, lb, sa prot, sb prot, v, v list) label ->
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
          ReceiveMany (a.role,
                       [(fun ks -> 
                            Lwt_list.map_s (fun k -> channel.receiver k) ks |>
                              Lwt.map (fun vs -> offer_label (vs, Lazy.force sb))
            )])
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

    let labelmany : type l r. (r, l) sendmany prot lazy_t -> conn -> l =
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
  end
end
