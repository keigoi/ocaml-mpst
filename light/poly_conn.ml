(* dune build light/light.exe && ./_build/default/light/light.exe *)
type ('v1, 'v2, 's1, 's2) plens = {
    get : 's1 -> 'v1;
    put : 's1 -> 'v2 -> 's2;
  }
type ('v, 's) lens = ('v, 'v, 's, 's) plens


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
  type ('s,'v,'r) send = DummySend of 's * 'v * 'r
  type ('s,'v,'r) recv = DummyRecv of 's * 'v * 'r
  type ('ss,'r) select = DummySelect of 'ss * 'r
  type ('ss,'r) offer = DummyOffer of 'ss * 'r
  type close
  type ('k, 'r) conn = DummyConn

  type (_,_) prot =
  | Send : 'r * ('k -> 'v Channel.o) * ('ks -> ('ks,'s) sess) -> ('ks, ('s, 'v, ('k,'r) conn) send) prot
  | Recv : 'r * (('k -> 'v Channel.i) * ('ks -> ('ks, 's) sess)) list -> ('ks, ('s, 'v, ('k,'r) conn) recv) prot
  | Select : 'r * ('ks -> 'k -> 'ss) -> ('ks,('ss, ('k,'r) conn) select) prot
  | Offer : 'r * ('ks -> 'k -> 'ss Channel.i) list -> ('ks, ('ss, ('k,'r) conn) offer) prot
  | Close : ('ks, close) prot

  and ('ks,'c) sess = Sess of 'ks * ('ks, 'c) prot

  val init: 'ks -> ('ks,'s) prot -> ('ks, 's) sess

  val send : <role:'r; lens:('k,'ks) lens; ..> -> 'v -> ('ks, ('s,'v,('k, 'r) conn) send) sess -> ('ks,'s) sess
  val receive : <role:'r; lens:('k,'ks) lens; ..> -> ('ks,('s,'v,('k,'r) conn) recv) sess -> 'v * ('ks,'s) sess
    
  val select_left :
    <role:'r; lens:('k,'ks) lens; ..> ->
    ('ks, (< left : ('ks,'s1) sess; .. >, ('k,'r) conn) select) sess ->
    ('ks, 's1) sess
  val select_right :
    <role:'r; lens:('k,'ks) lens; ..> ->
    ('ks, (< right : ('ks,'s1) sess; .. >, ('k,'r) conn) select) sess ->
    ('ks, 's1) sess
  val offer :
    <role:'r; lens:('k,'ks) lens; ..> ->
    ('ks, ([`left of ('ks,'s1) sess | `right of ('ks,'s2) sess],('k,'r) conn) offer) sess ->
    [`left of ('ks,'s1) sess | `right of ('ks,'s2) sess]
  val close : ('ks,close) sess -> unit
  
  (* session creation *)
  module Internal : sig
    (* merge operator for global description *)
    val merge : ('ks,'a) prot -> ('ks,'a) prot -> ('ks,'a) prot
  end

end = struct

  type ('a,'b) either = Left of 'a | Right of 'b

  type ('s,'v,'r) send = DummySend of 's * 'v * 'r
  type ('s,'v,'r) recv = DummyRecv of 's * 'v * 'r
  type ('ss,'r) select = DummySelect of 'ss * 'r
  type ('ss,'r) offer = DummyOffer of 'ss * 'r
  type close
  type ('k, 'r) conn = DummyConn

  type (_,_) prot =
  | Send : 'r * ('k -> 'v Channel.o) * ('ks -> ('ks,'s) sess) -> ('ks, ('s, 'v, ('k,'r) conn) send) prot
  | Recv : 'r * (('k -> 'v Channel.i) * ('ks -> ('ks, 's) sess)) list -> ('ks, ('s, 'v, ('k,'r) conn) recv) prot
  | Select : 'r * ('ks -> 'k -> 'ss) -> ('ks,('ss, ('k,'r) conn) select) prot
  | Offer : 'r * ('ks -> 'k -> 'ss Channel.i) list -> ('ks, ('ss, ('k,'r) conn) offer) prot
  | Close : ('ks, close) prot
  and ('ks,'c) sess = Sess of 'ks * ('ks, 'c) prot

  let init ks p = Sess (ks, p)
    
  let send =
    fun ridx v (Sess (ks,Send (_,f,cont))) ->
    let k = ridx#lens.get ks in
    let ch = f k in
    Channel.send ch v;
    cont ks
          
  let receive =
    fun ridx (Sess (ks,Recv (_,xs))) ->
    let k = ridx#lens.get ks in
    let xs = List.map (fun (f,s) -> f k, s) xs in
    let v,s = Channel.choose' xs in
    v, s ks
       
  let select_left ridx (Sess (ks, Select (_,f))) =
    (f ks (ridx#lens.get ks))#left
  
  let select_right ridx (Sess (ks, Select (_,f))) =
    (f ks (ridx#lens.get ks))#right
  
  let offer ridx (Sess (ks,Offer (_,xs))) =
    let k = ridx#lens.get ks in
    Channel.choose (List.map (fun f -> f ks k) xs)
       
  let close (Sess (_,_)) = ()
  
   module Internal = struct
    
    let merge : type t. ('ks,t) prot -> ('ks,t) prot -> ('ks,t) prot = fun x y ->
      match x, y with
      | Send (_,_,_), Send (_,_,_) ->
         failwith "role not enabled"
      | Select _, Select _ ->
         failwith "role not enabled"
      | Recv (r,xs), Recv (_,ys) ->
         Recv (r, xs @ ys)
      | Offer (r, xs), Offer (_, ys) ->
         Offer (r, xs @ ys)
      | Close, Close ->
         Close
  end
end

module Global : sig
  open Local
  type ('k1,'k2,'v) channel = {receiver: 'k1 -> 'v Channel.i; sender: 'k2 -> 'v Channel.o}
  type ('k1,'k2) medium = {instance : 'v. unit -> ('k1,'k2,'v) channel}
  
  val ( ==> ) :
        < role : 'ra;
          lens : (('ks,'sa) prot, ('ks, ('sa, 'v, ('k2, 'rb) conn) send) prot, 'c0, 'c1) plens;
          ext : 'ms -> 'ms0; .. > ->
        < role : 'rb;
          lens : (('ks, 'sb) prot, ('ks, ('sb, 'v, ('k1, 'ra) conn) recv) prot, 'c1, 'c2) plens;
          ext : 'ms0 -> ('k1, 'k2) medium; .. > ->
        ('ms -> 'c0) -> 'ms -> 'c2

  type ('ks1, 'ks2, 'sa, 'sb, 'sa0, 'sb0) label1 = {
    label1 :
      'k1 'k2 'ra 'rb.
        ('k1, 'k2, unit) channel ->
        ('ra * ('ks1, 'sa0) prot lazy_t) *
        ('rb * ('ks2, 'sb0) prot lazy_t) ->
        ('ks1, ('sa, ('k2, 'rb) conn) select) prot *
        ('ks2, ('sb, ('k1, 'ra) conn) offer) prot;
  }
  val left :
    ('ksa, 'ksb, < left : ('ksa, 'sa) sess >, [> `left of ('ksb, 'sb) sess ], 'sa, 'sb) label1
  val right :
    ('ksa, 'ksb, < right : ('ksa, 'sa) sess >, [> `right of ('ksb, 'sb) sess ], 'sa, 'sb) label1
     
  val ( --> ) :
    < role : 'ra;
      lens : (('ksa, 'sa) prot,
              ('ksa, ('la, ('k2, 'rb) conn) select) prot, 'c0, 'c1) plens;
      ext : 'ms -> 'ms0; .. > ->
    < role : 'rb;
      lens : (('ksb, 'sb) prot,
              ('ksb, ('lb, ('k1, 'ra) conn) offer) prot, 'c1, 'c2) plens;
      ext : 'ms0 -> ('k1, 'k2) medium; .. > ->
    ('ksa, 'ksb, 'la, 'lb, 'sa, 'sb) label1 ->
    ('ms -> 'c0) -> 'ms -> 'c2

  type ('l, 'r, 'lr) label_merge
  val left_or_right :
    (<left : ('ks, 'sl) sess>, <right : ('ks, 'sr) sess>, <left: ('ks, 'sl) sess; right: ('ks,'sr) sess>) label_merge
  val right_or_left :
    (<right : ('ks, 'sr) sess>, <left : ('ks, 'sl) sess>, <left: ('ks, 'sl) sess; right: ('ks,'sr) sess>) label_merge
    
  val choice_at :
    < lens : (('ks, close) prot, ('ks, ('lr, ('k, 'ra) conn) select) prot, 'c1, 'c2) plens; .. > ->
    ('l, 'r, 'lr) label_merge ->
    < lens : (('ks, ('l, ('k, 'ra) conn) select) prot, ('ks, close) prot, 'c0l, 'c1) plens;
      merge : 'c1 -> 'c1 -> 'c1; .. > * ('ms -> 'c0l) ->
    < lens : (('ks, ('r, ('k, 'ra) conn) select) prot, ('ks, close) prot, 'c0r, 'c1) plens; .. > * ('ms -> 'c0r) ->
    'ms -> 'c2
                                                                                      
end = struct
  open Local
  type ('k1,'k2,'v) channel = {receiver: 'k1 -> 'v Channel.i; sender: 'k2 -> 'v Channel.o}
  type ('k1,'k2) medium = {instance : 'v. unit -> ('k1,'k2,'v) channel}
     
  type ('ks1, 'ks2, 'sa,'sb,'sa0,'sb0) label1 =
    {label1:'k1 'k2 'ra 'rb. ('k1,'k2,unit) channel -> (('ra * ('ks1,'sa0) prot Lazy.t) * ('rb * ('ks2,'sb0) prot Lazy.t)
                      -> ('ks1,('sa, ('k2,'rb) conn) select) prot * ('ks2,('sb, ('k1,'ra) conn) offer) prot)}

  let left = {label1=(fun c ((ra,sa), (rb,sb)) ->
                Select (rb, fun ks k -> object method left=Channel.send (c.sender k) (); Sess (ks, Lazy.force sa) end),
                Offer (ra, [(fun ks k -> Channel.map (c.receiver k) (fun _ -> `left ((Sess (ks, Lazy.force sb)))))])
             )}
  let right = {label1=(fun c ((ra,sa), (rb,sb)) ->
                Select (rb, fun ks k -> object method right=Channel.send (c.sender k) (); Sess (ks, Lazy.force sa) end),
                Offer (ra, [(fun ks k -> Channel.map (c.receiver k) (fun _ -> `right ((Sess (ks, Lazy.force sb)))))])
             )}

  let (-->) a b ({label1}) c0 ms =
    let c0 = c0 ms in
    let m = b#ext (a#ext ms) in
    let io = m.instance () in
    let sa = lazy (a#lens.get c0) in
    let rec sb = lazy (b#lens.get (Lazy.force c1)) 
    and sab' = lazy (label1 io ((a#role, sa), (b#role, sb)))
    and c1 = lazy (a#lens.put c0 (fst (Lazy.force sab'))) in
    let c2 = b#lens.put (Lazy.force c1) (snd (Lazy.force sab')) in
    c2

  type ('l, 'r, 'lr) label_merge =
    {label_merge: 'rb 'ks 'k.
     (('ks, ('l, ('k,'rb) conn) select) prot -> ('ks, ('r,('k,'rb) conn) select) prot -> ('ks, ('lr, ('k,'rb) conn) select) prot)}
  
  let left_or_right : (<left:('ks,'sl) sess>, <right:('ks,'sr) sess>, <left:('ks,'sl) sess; right:('ks,'sr) sess>) label_merge =
    {label_merge=(fun (Select (rb,ol)) (Select (_,or_)) ->
         Select (rb, fun ks k -> object method left=(ol ks k)#left method right=(or_ ks k)#right end))}

  let right_or_left : (<right:('ks,'sr) sess>, <left:('ks,'sl) sess>, <left:('ks,'sl) sess; right:('ks,'sr) sess>) label_merge =
    {label_merge=(fun (Select (rb,or_)) (Select (_,ol)) ->
         Select (rb, fun ks k -> object method left=(ol ks k)#left method right=(or_ ks k)#right end))}
  
  
  let choice_at a {label_merge} (al,cl) (ar,cr) ms =
    let cl = cl ms and cr = cr ms in
    let sal, sar = al#lens.get cl, ar#lens.get cr in
    let cl, cr = al#lens.put cl Close, ar#lens.put cr Close in
    let c = al#merge cl cr in
    a#lens.put c (label_merge sal sar)
  
  let (==>) : 'ms 'ms0 'sa 'sb 'v 'k1 'k2 'ra 'rb 'c0 'c1 'c2.
        < ext : 'ms -> 'ms0;
          lens : (('ks1,'sa) prot, ('ks1, ('sa, 'v, ('k2, 'rb) conn) send) prot, 'c0, 'c1) plens;
          role : 'ra; .. > ->
        < ext : 'ms0 -> ('k1, 'k2) medium;
          lens : (('ks2, 'sb) prot, ('ks2, ('sb, 'v, ('k1, 'ra) conn) recv) prot, 'c1, 'c2) plens;
          role : 'rb; .. > ->
        ('ms -> 'c0) -> 'ms -> 'c2
        = fun a b c0 ms ->
    let c0 = c0 ms in
    let sa = lazy (a#lens.get c0) in
    let rec sb = lazy (b#lens.get (Lazy.force c1)) 
    and m = b#ext (a#ext ms)
    and io = lazy (m.instance ())
    and sab' = lazy (Send (b#role, (Lazy.force io).sender, (fun ks -> Sess (ks, Lazy.force sa))),
                     Recv (a#role, [(Lazy.force io).receiver, (fun ks -> Sess (ks, Lazy.force sb))]))
    and c1 = lazy (a#lens.put c0 (fst (Lazy.force sab'))) in
    let c2 = b#lens.put (Lazy.force c1) (snd (Lazy.force sab')) in
    c2
end

module ThreeParty = struct
  open Local
  let finish3 ms =
    Close, Close, Close

  type a = A
  type b = B
  type c = C

  open Internal
  let merge3 (x, y, z) (x', y', z') = merge x x', merge y y', merge z z'

  open Global

  let a = object method role=A; method lens={get=(fun (x,_,_) -> x); put=(fun (_,y,z) x->(x,y,z))}; method merge=merge3 method ext x=x#a end
  let b = object method role=B; method lens={get=(fun (_,y,_) -> y); put=(fun (x,_,z) y->(x,y,z))}; method merge=merge3 method ext x=x#b end
  let c = object method role=C; method lens={get=(fun (_,_,z) -> z); put=(fun (x,y,_) z->(x,y,z))}; method merge=merge3 method ext x=x#c end

  let f () = (a --> b) left @@ finish3
end

open Global
open ThreeParty
include Local

module Example1 = struct
  let global_example () =
    (a ==> b) @@
    (b ==> c) @@
    choice_at a left_or_right
      (a, (a --> b) left @@
          (b ==> c) @@
          finish3)
      (a, (a --> b) right @@
          (b ==> a) @@
          (b ==> c) @@
          finish3)

  let t1 sa =
    let s = sa in
    let s = send b 100 s in
    if Random.bool () then
      let s = select_left b s in
      close s
    else
      let s = select_right b s in
      let v,s = receive b s in
      Printf.printf "A: received: %s\n" v;
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
       let s = send a "to A" s in
       let s = send c "to C (2)" s in
       close s
       
  let t3 sc =
    let s = sc in
    let v,s = receive b s in
    Printf.printf "C: received: %s\n" v;
    let w,s = receive b s in
    Printf.printf "C: received: %s\n" w;
    close s

  let shmem =
    let m = {instance=(fun () -> let o,i = Channel.create () in {receiver=(fun () -> i); sender=(fun () -> o)})} in
    let make_ab p = object method a=p method b=p end in
    let make_bc p = object method b=p method c=p end in
    let make_ca p = object method a=p method c=p end in
    object method a=make_bc m method b=make_ca m method c=make_ab m end
    
  let main () =
    Random.self_init ();
    let g = global_example () shmem in
    let sa, sb, sc = a#lens.get g, b#lens.get g, c#lens.get g in
    let t1id = Thread.create (fun () -> t1 (init ((),(),()) sa)) () in
    let t2id = Thread.create (fun () -> t2 (init ((),(),()) sb)) () in
    t3 (init ((),(),()) sc);
    Thread.join t1id;
    Thread.join t2id;
    ()
end

let () = Example1.main()
