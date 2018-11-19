(* dune build light/light.exe && ./_build/default/light/light.exe *)

module Channel : sig
  type 'a o
  type +'a i
  val create : unit -> 'a o * 'a i
  val send : 'a o -> 'a -> unit
  val recv : 'a i -> 'a
  val choose : 'a i list -> 'a
  val choose' : ('a i * 'b) list -> 'a * 'b
  val map : 'a i -> ('a -> 'b) -> 'b i
end = struct
  type 'a o = 'a Event.channel
  type 'a i = 'a Event.event
  let create () =
    let ch = Event.new_channel () in
    ch, Event.receive ch 
  let send t v = Event.sync (Event.send t v)
  let recv t = Event.sync t
  let choose xs =
    Event.sync (Event.choose xs)
  let wrap (t,a) =
    Event.wrap t (fun v -> v, a)
  let choose' xs =
    Event.sync (Event.choose (List.map wrap xs))
  let map t f =
    Event.wrap t f
end

module Local : sig
  type ('a, 'b) either = Left of 'a | Right of 'b
  type ('s, 'v, 'r) send
  type ('s, 'v, 'r) recv
  type 'ss select
  type ('ss, 'r) offer
  type close
  type 'c sess
  val send : <role:'r; ..> -> 'v -> ('s,'v,'r) send sess -> 's sess
  val receive : <role:'r; ..> -> ('s,'v,'r) recv sess -> 'v * 's sess
    
  val select_left : (<ext : (< .. > as 'r) -> < left : 's1 sess; .. >; ..>) -> 'r select sess -> 's1 sess
  val select_right : (<ext : (< .. > as 'r) -> < right : 's1 sess; .. >; ..>) -> 'r select sess -> 's1 sess
  val offer : <role:'r; ..> -> ([`left of 's1 sess | `right of 's2 sess],'r) offer sess -> [`left of 's1 sess | `right of 's2 sess]
  val close : close sess -> unit

  type ('l, 'r, 'lr) select_merge =
    {select_merge:
     ('l select sess -> 'r select sess -> 'lr select sess)}

  val left_or_right :
    <role: 'rr;
     ext : (< .. > as 'l) -> < left : 'sl sess >;
     in_ : <left : 'sl sess; right : 'sr sess> -> (< .. > as 'lr); .. > ->
    <role: 'rr;
     ext : (< .. > as 'r) -> < right : 'sr sess >; .. > ->
    ('l, 'r, 'lr) select_merge

  val right_or_left :
    <role: 'rr;
     ext : (< .. > as 'r) -> < right : 'sr sess >;
     in_ : <left : 'sl sess; right : 'sr sess> -> (< .. > as 'lr); .. > ->
    <role: 'rr;
     ext : (< .. > as 'l) -> < left : 'sl sess >; .. > ->
    ('l, 'r, 'lr) select_merge

  val roles : 
    <ext : (< .. > as 'aa) -> 'a; .. > ->
    <ext : (< .. > as 'bb) -> 'b; .. > ->
    ('a -> 'b -> 'cc) ->
    ('aa, 'bb, 'cc) select_merge
     
  (* session creation *)
  module Internal : sig
    val s2c :
      from:'a * 'b sess lazy_t ->
      to_:'c * 'd sess lazy_t ->
      ('b, 'e, 'c) send sess * ('d, 'e, 'a) recv sess
    val s2c_left :
      from:< role : 'r1; .. > * 's1 sess lazy_t ->
      to_:< in_ : < left : 's1 sess > -> (< .. > as 'r); .. > * 's2 sess lazy_t ->
      'r select sess * ([> `left of 's2 sess ], 'r1) offer sess
    val s2c_right :
      from:< role : 'r1; .. > * 's1 sess lazy_t ->
      to_:< in_ : < right : 's1 sess > -> (< .. > as 'r); .. > *
          's2 sess lazy_t ->
      'r select sess * ([> `right of 's2 sess ], 'r1) offer sess
    val s2c_branch :
      from:< role : 'r1; .. > * ('sl1 sess lazy_t * 'sr1 sess lazy_t) ->
      to_:< in_ : < left : 'sl1 sess; right : 'sr1 sess > ->
                  (< .. > as 'r2_leftright);
            .. > *
          ('sl2 sess lazy_t * 'sr2 sess lazy_t) ->
      'r2_leftright select sess *
      ([> `left of 'sl2 sess | `right of 'sr2 sess ], 'r1) offer sess
    val end_ : close sess
    val merge : 't sess -> 't sess -> 't sess
    val select_merge :
      ('a, 'b, 'c) select_merge ->
      'a select sess -> 'b select sess -> 'c select sess
  end
end = struct

  type ('a,'b) either = Left of 'a | Right of 'b

  type ('s,'v,'r) send = DummySend of 's * 'v * 'r
  type ('s,'v,'r) recv = DummyRecv of 's * 'v * 'r

  type 'ss select = DummySelect of 'ss
  type ('ss,'r) offer = DummyOffer of 'ss * 'r

  type close

  type _ raw_sess =
  | Send : 'r * 'v Channel.o * 's raw_sess Lazy.t -> ('s, 'v, 'r) send raw_sess
  | Recv : 'r * ('v Channel.i * 's raw_sess Lazy.t) list -> ('s, 'v, 'r) recv raw_sess
  | Select : 'ss -> 'ss select raw_sess
  | Offer : 'r * 'ss Channel.i list -> ('ss, 'r) offer raw_sess
  | Close : close raw_sess

  type 'c sess = Sess of 'c raw_sess
  let unsess (Sess s) = s

  let send _ v (Sess s) =
    match s with
    | Send (_,ch,cont) ->
       Channel.send ch v;
       Sess (Lazy.force cont)
          
  let receive (_:'r) (Sess s) =
    match s with
    | Recv (_,xs) ->
       let v,s = Channel.choose' xs in
       v, Sess (Lazy.force s)
       
  let select_left r (Sess (Select s)) =
    (r#ext s)#left

  let select_right r (Sess (Select s)) =
    (r#ext s)#right

  let offer (_:'r) (Sess s) =
    match s with
    | Offer (_,xs) ->
       Channel.choose xs
       
  let close (Sess _) = ()

  type ('l, 'r, 'lr) select_merge =
    {select_merge:
     ('l select sess -> 'r select sess -> 'lr select sess)}

  let left_or_right :
    <role: 'rr;
     ext : (< .. > as 'l) -> < left : 'sl sess >;
     in_ : <left : 'sl sess; right : 'sr sess> -> (< .. > as 'lr); .. > ->
    <role: 'rr;
     ext : (< .. > as 'r) -> < right : 'sr sess >; .. > ->
    ('l, 'r, 'lr) select_merge =
      fun r1 r2 ->
      {select_merge=(fun (Sess (Select l)) (Sess (Select r)) ->
         Sess (Select (r1#in_ (object method left=(r1#ext l)#left method right=(r2#ext r)#right end))))}

  let right_or_left :
    <role: 'rr;
     ext : (< .. > as 'r) -> < right : 'sr sess >;
     in_ : <left : 'sl sess; right : 'sr sess> -> (< .. > as 'lr); .. > ->
    <role: 'rr;
     ext : (< .. > as 'l) -> < left : 'sl sess >; .. > ->
    ('l, 'r, 'lr) select_merge =
      fun r1 r2 ->
      {select_merge=(fun (Sess (Select l)) (Sess (Select r)) ->
         Sess (Select (r1#in_ (object method left=(r2#ext l)#left method right=(r1#ext r)#right end))))}

  let roles : 
    <ext : (< .. > as 'aa) -> 'a; .. > ->
    <ext : (< .. > as 'bb) -> 'b; .. > ->
    ('a -> 'b -> 'cc) ->
    ('aa, 'bb, 'cc) select_merge =
      fun a b f ->
      {select_merge=(fun (Sess (Select l)) (Sess (Select r)) -> Sess (Select (f (a#ext l) (b#ext r))))}

  let unsess' s = lazy (unsess (Lazy.force s))

  module Internal = struct
    
    let s2c ~from:(r1,s1) ~to_:(r2,s2) =
      let oc, ic = Channel.create () in
      Sess (Send (r2, oc, unsess' s1)), Sess (Recv (r1, [(ic, unsess' s2)]))

    let s2c_left : from:((<role:'r1; ..>) * 's1 sess Lazy.t) ->
              to_:(<in_:<left: 's1 sess> -> (< .. > as 'r); ..> * 's2 sess Lazy.t) ->
              'r select sess * ([>`left of 's2 sess], 'r1) offer sess = fun ~from:(r1,s1) ~to_:(r2,s2) ->
      let oc, ic = Channel.create () in
      Sess (Select (r2#in_ (object method left=Channel.send oc (); Lazy.force s1 end))),
      Sess (Offer (r1#role, [Channel.map ic (fun _ -> `left (Lazy.force s2))]))

    let s2c_right : from:((<role:'r1; ..>) * 's1 sess Lazy.t) ->
              to_:(<in_:<right: 's1 sess> -> (< .. > as 'r); ..> * 's2 sess Lazy.t) ->
              'r select sess * ([>`right of 's2 sess], 'r1) offer sess = fun ~from:(r1,s1) ~to_:(r2,s2) ->
      let oc, ic = Channel.create () in
      Sess (Select (r2#in_ (object method right=Channel.send oc (); Lazy.force s1 end))),
      Sess (Offer (r1#role, [Channel.map ic (fun _ -> `right (Lazy.force s2))]))

    let s2c_branch :
          from:((<role:'r1; ..>) * ('sl1 sess Lazy.t * 'sr1 sess Lazy.t)) ->
          to_:(<in_:<left : 'sl1 sess; right: 'sr1 sess> -> (< .. > as 'r2_leftright); ..>
              * ('sl2 sess Lazy.t * 'sr2 sess Lazy.t)) ->
          'r2_leftright select sess * ([>`left of 'sl2 sess | `right of 'sr2 sess], 'r1) offer sess =
      fun ~from:(r1,(sl1,sr1)) ~to_:(r2,(sl2,sr2)) ->
      let oc_l, ic_l = Channel.create () in
      let oc_r, ic_r = Channel.create () in
      Sess (Select (r2#in_ (object
                              method left=Channel.send oc_l (); Lazy.force sl1
                              method right=Channel.send oc_r (); Lazy.force sr1
                        end))),
      Sess (Offer (r1#role, [Channel.map ic_l (fun _ -> `left (Lazy.force sl2))
                            ;Channel.map ic_r (fun _ -> `right (Lazy.force sr2))
        ]))

    let end_ = Sess Close

    let merge : type t. t sess -> t sess -> t sess = fun (Sess x) (Sess y) ->
      match x, y with
      | Send (_,_,_), Send (_,_,_) ->
         failwith "role not enabled"
      | Select _, Select _ ->
         failwith "role not enabled"
      | Recv (r,xs), Recv (_,ys) ->
         Sess (Recv (r, xs @ ys))
      | Offer (r, xs), Offer (_, ys) ->
         Sess (Offer (r, xs @ ys))
      | Close, Close ->
         Sess Close

    let select_merge {select_merge} = select_merge
  end
end

module Global : sig
  type ('v1, 'v2, 's1, 's2) lens = {
    get : 's1 -> 'v1;
    put : 's1 -> 'v2 -> 's2;
  }
  type ('r, 'v1, 'v2, 's1, 's2) role = {
    role : 'r;
    lens : ('v1, 'v2, 's1, 's2) lens;
    merge : 's2 -> 's2 -> 's2;
  }
  type ('rr1, 'rr2, 'sa, 'sb, 'sa0, 'sb0) label1
                                     
  open Local
  
  val ( ==> ) :
    < lens : ('sa sess, ('sa, 'v, 'rb) send sess, 'c0, 'c1) lens;
      role : 'ra; .. > ->
    < lens : ('sb sess, ('sb, 'v, 'ra) recv sess, 'c1, 'c2) lens;
      role : 'rb; .. > ->
    'c0 -> 'c2
  
  val left :
    (< role : 'r1; .. >,
     < in_ : < left : 'sa0 sess > -> (< .. > as 'a); .. >,
     'a select, ([> `left of 'sb0 sess ], 'r1) offer, 'sa0,
     'sb0)
    label1
  val right :
    (< role : 'r1; .. >,
     < in_ : < right : 'sa0 sess > -> (< .. > as 'a); .. >,
     'a select, ([> `right of 'sb0 sess ], 'r1) offer,
     'sa0, 'sb0)
    label1

  val choice_at :
    < lens : (close sess, 's select sess, 'c2, 'c3) lens; .. > ->
    ('sl, 'sr, 's) select_merge ->
    < lens : ('sl select sess, close sess, 'c0l, 'c1l) lens;
      merge : 'c1l -> 'c1r -> 'c2; .. > *
    'c0l ->
    < lens : ('sr select sess, close sess, 'c0r, 'c1r) lens;
      .. > *
    'c0r -> 'c3
  
  val ( --> ) :
    (< lens : ('sa0 sess, 'sa sess, 'c0, 'c1) lens; .. > as 'a) ->
    (< lens : ('sb0 sess, 'sb sess, 'c1, 'c2) lens; .. > as 'b) ->
    ('a, 'b, 'sa, 'sb, 'sa0, 'sb0) label1 -> 'c0 -> 'c2

end = struct

  type ('v1, 'v2, 's1, 's2) lens = {
    get : 's1 -> 'v1;
    put : 's1 -> 'v2 -> 's2;
  }
  type ('r, 'v1, 'v2, 's1, 's2) role = {
    role : 'r;
    lens : ('v1, 'v2, 's1, 's2) lens;
    merge : 's2 -> 's2 -> 's2;
    }
  open Local
  open Local.Internal
  type ('rr1, 'rr2, 'sa, 'sb, 'sa0, 'sb0) label1 =
    {label1:
     (('rr1 * 'sa0 sess lazy_t) * ('rr2 * 'sb0 sess lazy_t)) -> 'sa sess * 'sb sess}
    
  let left : (<role:'r1; ..>, <in_:<left : 'sa0 sess> -> 'sa; ..>,
              'sa select, ([> `left of 'sb0 sess], 'r1) offer , 'sa0, 'sb0) label1 =
    {label1 = (fun ((a,sa0), (b,sb0)) -> s2c_left ~from:(a,sa0) ~to_:(b,sb0))}
    
  let right : (<role:'r1; ..>, <in_:<right : 'sa0 sess> -> 'sa; ..>,
              'sa select, ([> `right of 'sb0 sess], 'r1) offer , 'sa0, 'sb0) label1 =
    {label1 = (fun ((a,sa0), (b,sb0)) -> s2c_right ~from:(a,sa0) ~to_:(b,sb0))}
    
  let (-->) a b {label1} c0 =
    let (sa : _ sess lazy_t) = lazy (a#lens.get c0) in
    let rec sb = (lazy (b#lens.get (Lazy.force c1)) : _ sess lazy_t)
    and sab' = lazy (label1 ((a, sa), (b, sb)))
    and c1 = lazy (a#lens.put c0 (fst (Lazy.force sab'))) in
    let c2 = b#lens.put (Lazy.force c1) (snd (Lazy.force sab')) in
    c2

  let choice_at a lm (al,cl) (ar,cr) =
    let sal, sar = al#lens.get cl, ar#lens.get cr in
    let cl, cr = al#lens.put cl end_, ar#lens.put cr end_ in
    let c = al#merge cl cr in
    a#lens.put c (select_merge lm sal sar)

  let (==>) a b c0 =
    let sa = lazy (a#lens.get c0) in
    let rec sb = lazy (b#lens.get (Lazy.force c1)) 
    and sab' = lazy (s2c ~from:(a#role, sa) ~to_:(b#role, sb))
    and c1 = lazy (a#lens.put c0 (fst (Lazy.force sab'))) in
    let c2 = b#lens.put (Lazy.force c1) (snd (Lazy.force sab')) in
    c2

  let (-%%->) a b ((al,bl),cl) ((ar,br),cr) =
    let sal, sar = lazy (al.lens.get cl), lazy (ar.lens.get cr) in
    let cl, cr = al.lens.put cl end_, ar.lens.put cr end_ in
    let sbl, sbr = lazy (bl.lens.get cl), lazy (br.lens.get cr) in
    let cl, cr = bl.lens.put cl end_, br.lens.put cr end_ in
    let c = bl.merge cl cr in
    let sa, sb = s2c_branch ~from:(a.role, (sal,sar)) ~to_:(b.role, (sbl,sbr)) in
    let c = a.lens.put c sa in
    let c = b.lens.put c sb in
    c
end

module ThreeParty = struct
  open Local.Internal

  let finish3 =
    end_, end_, end_

  type a = A
  type b = B
  type c = C

  let merge3 (x, y, z) (x', y', z') = merge x x', merge y y', merge z z'

  open Global

  let a = object method role=A method lens={get=(fun (x,_,_) -> x); put=(fun (_,y,z) x->(x,y,z))}; method merge=merge3 method in_ x = object method a=x end method ext x = x#a end
  let b = object method role=B method lens={get=(fun (_,y,_) -> y); put=(fun (x,_,z) y->(x,y,z))}; method merge=merge3 method in_ x = object method b=x end method ext x = x#b end
  let c = object method role=C method lens={get=(fun (_,_,z) -> z); put=(fun (x,y,_) z->(x,y,z))}; method merge=merge3 method in_ x = object method c=x end method ext x = x#c end
end

open Global
open ThreeParty
include Local

module Example2 = struct
  let global_example0 () = (* unused *)
    (a ==> b) @@
    (b ==> c) @@
    choice_at a (left_or_right b b)
      (a, (a --> b) left @@
          (b --> c) left @@
          (b ==> c) @@
          finish3)
      (a, (a --> b) right @@
          (b --> c) right @@
          (b ==> a) @@
          (c ==> a) @@
          finish3)

  let global_example () =
    (a ==> b) @@
    (b ==> c) @@
    choice_at a (roles b c (fun x y -> object method b=x method c=y end))
      (a, (a --> b) left @@
          (a --> c) left @@
          (b ==> c) @@
          finish3)
      (a, (a --> c) right @@
          (a --> b) right @@
          (b ==> a) @@
          (c ==> a) @@
          finish3)

  let t1 sa =
    let s = sa in
    let s = send b 100 s in
    if Random.bool () then
      let s = select_left b s in
      let s = select_left c s in
      close s
    else
      let s = select_right c s in
      let s = select_right b s in
      let v1,s = receive b s in
      Printf.printf "A: received from B: %s\n" v1;
      let v2,s = receive c s in
      Printf.printf "A: received from C: %s\n" v2;
      close s
      
  let t2 sb =
    let s = sb in
    let v,s = receive a s in
    Printf.printf "B: received: %d\n" v;
    let s = send c "Hello" s in
    match offer a s with
    | `left s ->
       let s = send c "to C (1)" s in
       close s
    | `right s ->
       let s = send a "to A (2)" s in
       close s
       
  let t3 sc =
    let s = sc in
    let v,s = receive b s in
    Printf.printf "C: received: %s\n" v;
    match offer a s with
    | `left s ->
      let w,s = receive b s in
      Printf.printf "C: received: %s\n" w;
      close s
    | `right s ->
      let s = send a "to A (2)" s in
      close s
    
  let main () =
    Random.self_init ();
    let g = global_example () in
    let sa, sb, sc = a#lens.get g, b#lens.get g, c#lens.get g in
    let t1id = Thread.create (fun () -> t1 sa) () in
    let t2id = Thread.create (fun () -> t2 sb) () in
    t3 sc;
    Thread.join t1id;
    Thread.join t2id;
    ()
end
                
let () = Example2.main ()
