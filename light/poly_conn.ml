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
  type ('ss, 'r) select
  type ('ss, 'r) offer
  type close
  type 'c sess
  val send : 'r -> 'v -> ('s,'v,'r) send sess -> 's sess
  val receive : 'r -> ('s,'v,'r) recv sess -> 'v * 's sess
    
  val select_left : 'r -> (< left : 's1 sess; .. >,'r) select sess -> 's1 sess
  val select_right : 'r -> (< right : 's2 sess; ..>,'r) select sess -> 's2 sess
  val offer : 'r -> ([`left of 's1 sess | `right of 's2 sess],'r) offer sess -> [`left of 's1 sess | `right of 's2 sess]
  val close : close sess -> unit

  type ('l, 'r, 'lr) label_merge

  val left_or_right :
    (<left : 'sl sess>, <right : 'sr sess>, <left: 'sl sess; right: 'sr sess>) label_merge

  val right_or_left :
    (<right : 'sr sess>, <left : 'sl sess>, <left: 'sl sess; right: 'sr sess>) label_merge
    
  (* session creation *)
  module Internal : sig
    val s2c : from:('r1 * 's1 sess Lazy.t) ->
              to_:('r2 * 's2 sess Lazy.t) ->
              ('s1, 'v, 'r2) send sess * ('s2, 'v, 'r1) recv sess
    val s2c_left : from:('r1 * 's1 sess Lazy.t) ->
              to_:('r2 * 's2 sess Lazy.t) ->
              (<left: 's1 sess>, 'r2) select sess * ([>`left of 's2 sess], 'r1) offer sess
    val s2c_right : from:('r1 * 's1 sess Lazy.t) ->
              to_:('r2 * 's2 sess Lazy.t) ->
              (<right: 's1 sess>, 'r2) select sess * ([>`right of 's2 sess], 'r1) offer sess
    val s2c_branch :
      from:('r1 * ('sl1 sess Lazy.t * 'sr1 sess Lazy.t)) ->
      to_:('r2 * ('sl2 sess Lazy.t * 'sr2 sess Lazy.t)) ->
      (<left: 'sl1 sess; right: 'sr1 sess>, 'r2) select sess * ([>`left of 'sl2 sess | `right of 'sr2 sess], 'r1) offer sess
    val end_ : close sess

    (* merge operator for global description *)
    val merge : 'a sess -> 'a sess -> 'a sess

    val label_merge :
      ('l, 'r, 'lr) label_merge ->
      ('l, 'rb) select sess ->
      ('r,'rb) select sess ->
      ('lr, 'rb) select sess
  end

end = struct

  type ('a,'b) either = Left of 'a | Right of 'b

  type ('s,'v,'r) send = DummySend of 's * 'v * 'r
  type ('s,'v,'r) recv = DummyRecv of 's * 'v * 'r

  type ('ss,'r) select = DummySelect of 'ss * 'r
  type ('ss,'r) offer = DummyOffer of 'ss * 'r

  type close

  type _ raw_sess =
  | Send : 'r * 'v Channel.o * 's raw_sess Lazy.t -> ('s, 'v, 'r) send raw_sess
  | Recv : 'r * ('v Channel.i * 's raw_sess Lazy.t) list -> ('s, 'v, 'r) recv raw_sess
  | Select : 'r * 'ss -> ('ss, 'r) select raw_sess
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
       
  let select_left r (Sess (Select (_,s))) =
    s#left

  let select_right r (Sess (Select (_,s))) =
    s#right

  let offer (_:'r) (Sess s) =
    match s with
    | Offer (_,xs) ->
       Channel.choose xs
       
  let close (Sess _) = ()

  type ('l, 'r, 'lr) label_merge =
    {label_merge: 'rb.
     (('l, 'rb) select sess -> ('r,'rb) select sess -> ('lr, 'rb) select sess)}

  let left_or_right :
    (<left : 'sl sess>, <right : 'sr sess>, <left: 'sl sess; right: 'sr sess>) label_merge =
    {label_merge=
       (fun (Sess (Select (rb,ol))) (Sess (Select (_,or_))) ->
         Sess (Select (rb, object method left=ol#left method right=or_#right end)))}

  let right_or_left :
    (<right : 'sr sess>, <left : 'sl sess>, <left: 'sl sess; right: 'sr sess>) label_merge =
    {label_merge=
       (fun (Sess (Select (rb,or_))) (Sess (Select (_,ol))) ->
         Sess (Select (rb, object method left=ol#left method right=or_#right end)))}

  let unsess' s = lazy (unsess (Lazy.force s))

  module Internal = struct
    
    let s2c ~from:(r1,s1) ~to_:(r2,s2) =
      let oc, ic = Channel.create () in
      Sess (Send (r2, oc, unsess' s1)), Sess (Recv (r1, [(ic, unsess' s2)]))

    let s2c_left ~from:(r1,s1) ~to_:(r2,s2) =
      let oc, ic = Channel.create () in
      Sess (Select (r2, (object method left=Channel.send oc (); Lazy.force s1 end))),
      Sess (Offer (r1, [Channel.map ic (fun _ -> `left (Lazy.force s2))]))

    let s2c_right ~from:(r1,s1) ~to_:(r2,s2) =
      let oc, ic = Channel.create () in
      Sess (Select (r2, (object method right=Channel.send oc (); Lazy.force s1 end))),
      Sess (Offer (r1, [Channel.map ic (fun _ -> `right (Lazy.force s2))]))

    let s2c_branch ~from:(r1,(sl1,sr1)) ~to_:(r2,(sl2,sr2)) =
      let oc_l, ic_l = Channel.create () in
      let oc_r, ic_r = Channel.create () in
      Sess (Select (r2,
                    (object
                       method left=Channel.send oc_l (); Lazy.force sl1
                       method right=Channel.send oc_r (); Lazy.force sr1 end))),
      Sess (Offer (r1, [Channel.map ic_l (fun _ -> `left (Lazy.force sl2));
                        Channel.map ic_r (fun _ -> `right (Lazy.force sr2))]))

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

    let label_merge {label_merge} = label_merge
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
  type ('sa0, 'sb0, 'sa, 'sb) label1
                                     
  open Local
  
  val ( ==> ) :
    ('ra, 'sa sess, ('sa, 'v, 'rb) send sess, 'c0, 'c1) role ->
    ('rb, 'sb sess, ('sb, 'v, 'ra) recv sess, 'c1, 'c2) role ->
    'c0 -> 'c2
  
  val left : (<left: 'sa sess>, [>`left of 'sb sess], 'sa, 'sb) label1
  val right : (<right: 'sa sess>, [>`right of 'sb sess], 'sa, 'sb) label1

  val choice_at :
    ('ra, close sess, ('lr, 'rb) select sess, 'c1, 'c2) role ->
    ('l, 'r, 'lr) label_merge ->
    ('ra, ('l, 'rb) select sess, close sess, 'cl0, 'c1) role * 'cl0 ->
    ('ra, ('r, 'rb) select sess, close sess, 'cr0, 'c1) role * 'cr0 -> 'c2
  
  val ( --> ) :
    ('ra, 'sa0 sess, ('sa, 'rb) select sess, 'c0, 'c1) role ->
    ('rb, 'sb0 sess, ('sb, 'ra) offer sess, 'c1, 'c2) role ->
    ('sa, 'sb, 'sa0, 'sb0) label1 ->
    'c0 -> 'c2
  
  val ( -%%-> ) :
    ('ra, close sess, (<left: 'sal sess; right: 'sar sess>, 'rb) select sess, 'c2, 'c3) role ->
    ('rb, close sess, ([>`left of 'sbl sess| `right of 'sbr sess], 'ra) offer sess, 'c3, 'c4) role ->
    (('ra, 'sal sess, close sess, 'cl0, 'cl1) role *
     ('rb, 'sbl sess, close sess, 'cl1, 'c2) role) *
    'cl0 ->
    (('ra, 'sar sess, close sess, 'cr0, 'cr1) role *
     ('rb, 'sbr sess, close sess, 'cr1, 'c2) role) *
    'cr0 -> 'c4

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
     
  type ('sa,'sb,'sa0,'sb0) label1 =
    {label1:'ra 'rb. (('ra * 'sa0 sess Lazy.t) * ('rb * 'sb0 sess Lazy.t)
                      -> ('sa, 'rb) select sess * ('sb, 'ra) offer sess)}

  open Local.Internal

  let left = {label1=(fun (from, to_) -> s2c_left ~from ~to_)}
  let right = {label1=(fun (from, to_) -> s2c_right ~from ~to_)}

  let (-->) a b ({label1}) c0 =
    let sa = lazy (a.lens.get c0) in
    let rec sb = lazy (b.lens.get (Lazy.force c1)) 
    and sab' = lazy (label1 ((a.role, sa), (b.role, sb)))
    and c1 = lazy (a.lens.put c0 (fst (Lazy.force sab'))) in
    let c2 = b.lens.put (Lazy.force c1) (snd (Lazy.force sab')) in
    c2

  let choice_at a lm (al,cl) (ar,cr) =
    let sal, sar = al.lens.get cl, ar.lens.get cr in
    let cl, cr = al.lens.put cl end_, ar.lens.put cr end_ in
    let c = al.merge cl cr in
    a.lens.put c (label_merge lm sal sar)

  let (==>) a b c0 =
    let sa = lazy (a.lens.get c0) in
    let rec sb = lazy (b.lens.get (Lazy.force c1)) 
    and sab' = lazy (s2c ~from:(a.role, sa) ~to_:(b.role, sb))
    and c1 = lazy (a.lens.put c0 (fst (Lazy.force sab'))) in
    let c2 = b.lens.put (Lazy.force c1) (snd (Lazy.force sab')) in
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

  let a = {role=A; lens={get=(fun (x,_,_) -> x); put=(fun (_,y,z) x->(x,y,z))}; merge=merge3}
  let b = {role=B; lens={get=(fun (_,y,_) -> y); put=(fun (x,_,z) y->(x,y,z))}; merge=merge3}
  let c = {role=C; lens={get=(fun (_,_,z) -> z); put=(fun (x,y,_) z->(x,y,z))}; merge=merge3}
end

open Global
open ThreeParty
include Local

module Example1 = struct
  let global_example () =
    (a ==> b) @@
    (b ==> c) @@
    (a -%%-> b)
      ((a,b), (b ==> c) @@
              finish3)
      ((a,b), (b ==> a) @@
              (b ==> c) @@
              finish3)

  let t1 sa =
    let s = sa in
    let s = send B 100 s in
    if Random.bool () then
      let s = select_left B s in
      close s
    else
      let s = select_right B s in
      let v,s = receive B s in
      Printf.printf "A: received: %s\n" v;
      close s
      
  let t2 sb =
    let s = sb in
    let v,s = receive A s in
    Printf.printf "B: received: %d\n" v;
    let s = send C "Hello" s in
    match offer A s with
    | `left s ->
       let s = send C "to C (1)" s in
       close s
    | `right s ->
       let s = send A "to A" s in
       let s = send C "to C (2)" s in
       close s
       
  let t3 sc =
    let s = sc in
    let v,s = receive B s in
    Printf.printf "C: received: %s\n" v;
    let w,s = receive B s in
    Printf.printf "C: received: %s\n" w;
    close s
    
  let main () =
    Random.self_init ();
    let g = global_example () in
    let sa, sb, sc = a.lens.get g, b.lens.get g, c.lens.get g in
    let t1id = Thread.create (fun () -> t1 sa) () in
    let t2id = Thread.create (fun () -> t2 sb) () in
    t3 sc;
    Thread.join t1id;
    Thread.join t2id;
    ()
end

module Example2 = struct
  let global_example () =
    (a ==> b) @@
    (b ==> c) @@
    choice_at a left_or_right
      (a, (a --> b) left @@
          (b --> c) left @@
          (b ==> c) @@
          finish3)
      (a, (a --> b) right @@
          (b --> c) right @@
          (b ==> a) @@
          (c ==> a) @@
          finish3)

  let t1 sa =
    let s = sa in
    let s = send B 100 s in
    if Random.bool () then
      let s = select_left B s in
      close s
    else
      let s = select_right B s in
      let v1,s = receive B s in
      Printf.printf "A: received from B: %s\n" v1;
      let v2,s = receive C s in
      Printf.printf "A: received from C: %s\n" v2;
      close s
      
  let t2 sb =
    let s = sb in
    let v,s = receive A s in
    Printf.printf "B: received: %d\n" v;
    let s = send C "Hello" s in
    match offer A s with
    | `left s ->
       let s = select_left C s in
       let s = send C "to C (1)" s in
       close s
    | `right s ->
       let s = select_right C s in
       let s = send A "to A (2)" s in
       close s
       
  let t3 sc =
    let s = sc in
    let v,s = receive B s in
    Printf.printf "C: received: %s\n" v;
    match offer B s with
    | `left s ->
      let w,s = receive B s in
      Printf.printf "C: received: %s\n" w;
      close s
    | `right s ->
      let s = send A "to A (2)" s in
      close s
    
  let main () =
    Random.self_init ();
    let g = global_example () in
    let sa, sb, sc = a.lens.get g, b.lens.get g, c.lens.get g in
    let t1id = Thread.create (fun () -> t1 sa) () in
    let t2id = Thread.create (fun () -> t2 sb) () in
    t3 sc;
    Thread.join t1id;
    Thread.join t2id;
    ()
end
                
let () = Example2.main ()
