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
  type ('s,'v,'r) send = DummySend
  type ('s,'v,'r) recv = DummyRecv
  type ('ss,'r) select = DummySelect
  type ('ss,'r) offer = DummyOffer
  type ('s,'ks,'r) connect = DummyConnect
  type close
  type ('k, 'r) conn = DummyConn

  type (_,_) prot =
  | Connect : 'r * ('ks2 -> ('ks2,'s) sess) -> ('ks, ('s, 'ks2, 'r) connect) prot
  | Send : 'r * ('k -> 'v -> unit) * ('ks -> ('ks,'s) sess) -> ('ks, ('s, 'v, ('k,'r) conn) send) prot
  | Recv : 'r * (('k -> 'v Channel.i) * ('ks -> ('ks, 's) sess)) list -> ('ks, ('s, 'v, ('k,'r) conn) recv) prot
  | Select : 'r * ('ks -> 'k -> 'ss) -> ('ks,('ss, ('k,'r) conn) select) prot
  | Offer : 'r * ('ks -> 'k -> 'ss Channel.i) list -> ('ks, ('ss, ('k,'r) conn) offer) prot
  | Close : ('ks, close) prot
  and ('ks,'c) sess =
    Sess of 'ks * ('ks, 'c) prot

  val init: 'ks -> ('ks,'s) prot -> ('ks, 's) sess

  val connect :
        <role:'r; lens:(unit,'k,'ks1,'ks2) plens; ..> ->
        'k ->
        ('ks1, ('s, 'ks2, 'r) connect) sess ->
        ('ks2, 's) sess
      
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

  type ('a, 'b) either = Left of 'a | Right of 'b
  type ('s,'v,'r) send = DummySend
  type ('s,'v,'r) recv = DummyRecv
  type ('ss,'r) select = DummySelect
  type ('ss,'r) offer = DummyOffer
  type ('s,'ks,'r) connect = DummyConnect
  type close
  type ('k, 'r) conn = DummyConn

  type (_,_) prot =
  | Connect : 'r * ('ks2 -> ('ks2,'s) sess) -> ('ks, ('s, 'ks2, 'r) connect) prot
  | Send : 'r * ('k -> 'v -> unit) * ('ks -> ('ks,'s) sess) -> ('ks, ('s, 'v, ('k,'r) conn) send) prot
  | Recv : 'r * (('k -> 'v Channel.i) * ('ks -> ('ks, 's) sess)) list -> ('ks, ('s, 'v, ('k,'r) conn) recv) prot
  | Select : 'r * ('ks -> 'k -> 'ss) -> ('ks,('ss, ('k,'r) conn) select) prot
  | Offer : 'r * ('ks -> 'k -> 'ss Channel.i) list -> ('ks, ('ss, ('k,'r) conn) offer) prot
  | Close : ('ks, close) prot
  and ('ks,'c) sess =
    Sess of 'ks * ('ks, 'c) prot

  let init ks p = Sess (ks, p)
                
  type none
  let connect : 'r 'k 'ks1 'ks2.
        <role:'r; lens:(unit,'k,'ks1,'ks2) plens; ..> ->
        'k ->
        ('ks1, ('s, 'ks2, 'r) connect) sess ->
        ('ks2, 's) sess =
          fun ridx k (Sess (ks, Connect (r, f))) ->
          let ks2 = ridx#lens.put ks k in
          f ks2
          
  let send =
    fun ridx v (Sess (ks,Send (_,writer,cont))) ->
    let k = ridx#lens.get ks in
    writer k v;
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

module Global: sig
  open Local
  type ('k1,'k2,'v) channel = {receiver: 'k1 -> 'v Channel.i; sender: 'k2 -> 'v -> unit}
  
  val ( ==> ) :
        < role : 'ra;
          lens : (('ksa,'sa) prot, ('ksa, ('sa, 'v, ('k2, 'rb) conn) send) prot, 'c0, 'c1) plens; .. > ->
        < role : 'rb;
          lens : (('ksb, 'sb) prot, ('ksb, ('sb, 'v, ('k1, 'ra) conn) recv) prot, 'c1, 'c2) plens; .. > ->
        < value : ('k1,'k2, 'v) channel; .. > ->
        'c0 -> 'c2

  type ('ks1, 'ks2, 'k1, 'k2, 'sa, 'sb, 'sa0, 'sb0) label1 = {
    label1 :
      'ra 'rb.
        ('ra * ('ks1, 'sa0) prot lazy_t) *
        ('rb * ('ks2, 'sb0) prot lazy_t) ->
        ('ks1, ('sa, ('k2, 'rb) conn) select) prot *
        ('ks2, ('sb, ('k1, 'ra) conn) offer) prot;
  }
  val left :
    <left:('k1,'k2,unit) channel; ..> ->
    ('ksa, 'ksb, 'k1, 'k2, < left : ('ksa, 'sa) sess >, [> `left of ('ksb, 'sb) sess ], 'sa, 'sb) label1
  val right :
    <right:('k1,'k2,unit) channel; ..> ->
    ('ksa, 'ksb, 'k1, 'k2, < right : ('ksa, 'sa) sess >, [> `right of ('ksb, 'sb) sess ], 'sa, 'sb) label1
     
  val ( --> ) :
    < role : 'ra;
      lens : (('ksa, 'sa) prot,
              ('ksa, ('la, ('k2, 'rb) conn) select) prot, 'c0, 'c1) plens; .. > ->
    < role : 'rb;
      lens : (('ksb, 'sb) prot,
              ('ksb, ('lb, ('k1, 'ra) conn) offer) prot, 'c1, 'c2) plens; .. > ->
    ('ksa, 'ksb, 'k1, 'k2, 'la, 'lb, 'sa, 'sb) label1 ->
    'c0 -> 'c2

  type ('l, 'r, 'lr) label_merge
  val left_or_right :
    (<left : ('ks, 'sl) sess>, <right : ('ks, 'sr) sess>, <left: ('ks, 'sl) sess; right: ('ks,'sr) sess>) label_merge
  val right_or_left :
    (<right : ('ks, 'sr) sess>, <left : ('ks, 'sl) sess>, <left: ('ks, 'sl) sess; right: ('ks,'sr) sess>) label_merge
    
  val choice_at :
    < lens : (('ks, close) prot, ('ks, ('lr, ('k, 'ra) conn) select) prot, 'c1, 'c2) plens; .. > ->
    ('l, 'r, 'lr) label_merge ->
    < lens : (('ks, ('l, ('k, 'ra) conn) select) prot, ('ks, close) prot, 'c0l, 'c1) plens;
      merge : 'c1 -> 'c1 -> 'c1; .. > * 'c0l ->
    < lens : (('ks, ('r, ('k, 'ra) conn) select) prot, ('ks, close) prot, 'c0r, 'c1) plens; .. > * 'c0r ->
    'c2

  val (=!=>) : 
    < role : 'ra;
      lens : (('ks11, 'sa) prot, ('ks1, ('sa, 'ks11, 'rb) connect) prot, 'c0, 'c1) plens; .. > ->
    < role : 'rb; 
      lens : (('ks22, 'sb) prot, ('ks2, ('sb, 'ks22, 'ra) connect) prot, 'c1, 'c2) plens; .. > ->
    'c0 -> 'c2
                                                                                      
end =  struct
  open Local
  type ('k1,'k2,'v) channel = {receiver: 'k1 -> 'v Channel.i; sender: 'k2 -> 'v -> unit}
     
  type ('ks1, 'ks2, 'k1, 'k2, 'sa,'sb,'sa0,'sb0) label1 =
    {label1: 'ra 'rb. (('ra * ('ks1,'sa0) prot Lazy.t) * ('rb * ('ks2,'sb0) prot Lazy.t)
                      -> ('ks1,('sa, ('k2,'rb) conn) select) prot * ('ks2,('sb, ('k1,'ra) conn) offer) prot)}

  let left m = {label1=(fun ((ra,sa), (rb,sb)) ->
                let c = m#left in
                Select (rb, fun ks k -> object method left=c.sender k (); Sess (ks, Lazy.force sa) end),
                Offer (ra, [(fun ks k -> Channel.map (c.receiver k) (fun _ -> `left ((Sess (ks, Lazy.force sb)))))])
             )}
  let right m = {label1=(fun ((ra,sa), (rb,sb)) ->
                let c = m#right in
                Select (rb, fun ks k -> object method right=c.sender k (); Sess (ks, Lazy.force sa) end),
                Offer (ra, [(fun ks k -> Channel.map (c.receiver k) (fun _ -> `right ((Sess (ks, Lazy.force sb)))))])
             )}

  let (-->) a b ({label1}) c0 =
    let sa = lazy (a#lens.get c0) in
    let rec sb = lazy (b#lens.get (Lazy.force c1)) 
    and sab' = lazy (label1 ((a#role, sa), (b#role, sb)))
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
  
  
  let choice_at a {label_merge} (al,cl) (ar,cr) =
    let sal, sar = al#lens.get cl, ar#lens.get cr in
    let cl, cr = al#lens.put cl Close, ar#lens.put cr Close in
    let c = al#merge cl cr in
    a#lens.put c (label_merge sal sar)
  
  let (==>) : 'sa 'sb 'v 'k1 'k2 'ra 'rb 'c0 'c1 'c2.
        < lens : (('ks1,'sa) prot, ('ks1, ('sa, 'v, ('k2, 'rb) conn) send) prot, 'c0, 'c1) plens;
          role : 'ra; .. > ->
        < lens : (('ks2, 'sb) prot, ('ks2, ('sb, 'v, ('k1, 'ra) conn) recv) prot, 'c1, 'c2) plens;
          role : 'rb; .. > ->
        <value:('k1,'k2,'v) channel;..> ->
        'c0 -> 'c2
        = fun a b m c0 ->
    let sa = lazy (a#lens.get c0) in
    let rec sb = lazy (b#lens.get (Lazy.force c1)) 
    and io = lazy (m#value)
    and sab' = lazy (Send (b#role, (Lazy.force io).sender, (fun ks -> Sess (ks, Lazy.force sa))),
                     Recv (a#role, [(Lazy.force io).receiver, (fun ks -> Sess (ks, Lazy.force sb))]))
    and c1 = lazy (a#lens.put c0 (fst (Lazy.force sab'))) in
    let c2 = b#lens.put (Lazy.force c1) (snd (Lazy.force sab')) in
    c2
  
  let (=!=>) : 'ra 'ra 'ks1 'ks11 'ks2 'ks22 'sa 'sb 'c0 'c1 'c2.
    < role : 'ra;
      lens : (('ks11, 'sa) prot, ('ks1, ('sa, 'ks11, 'rb) connect) prot, 'c0, 'c1) plens; .. > ->
    < role : 'rb; 
      lens : (('ks22, 'sb) prot, ('ks2, ('sb, 'ks22, 'ra) connect) prot, 'c1, 'c2) plens; .. > ->
    'c0 -> 'c2
        = fun a b c0 ->
    let sa = lazy (a#lens.get c0) in
    let rec sb = lazy (b#lens.get (Lazy.force c1)) 
    and sab' = lazy (Connect (b#role, (fun ks -> Sess (ks, Lazy.force sa))),
                     Connect (a#role, (fun ks -> Sess (ks, Lazy.force sb))))
    and c1 = lazy (a#lens.put c0 (fst (Lazy.force sab'))) in
    let c2 = b#lens.put (Lazy.force c1) (snd (Lazy.force sab')) in
    c2
end

module ThreeParty = struct
  open Local
  let finish3 =
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

  let shmem =
    let cre () =
      let o,i = Channel.create() in
      {sender=(fun () -> Channel.send o); receiver=(fun () -> i)}
    in
    object
      method value = cre ()
      method left = cre ()
      method right = cre ()
    end

  let f () = (a --> b) (left shmem) @@ finish3
end

open Global
open ThreeParty
include Local

module Example1 = struct
  let global_example ab ba bc =
    (a =!=> b) @@
    (b =!=> c) @@
    (a ==> b) ab @@
    (b ==> c) bc @@
    choice_at a left_or_right
      (a, (a --> b) (left ab) @@
          (b ==> c) bc @@
          finish3)
      (a, (a --> b) (right ab) @@
          (b ==> a) ba @@
          (b ==> c) bc @@
          finish3)

  let t1 sa =
    let s = sa in
    let s = connect b () s in
    let s = send b 100 s in
    if Random.bool () then
      let s = select_left b s in
      close s
    else
      let s = select_right b s in
      let v,s = receive b s in
      Printf.printf "A: received: %s\n" v;
      close s
      
  let t2 bc_channel sb =
    let s = sb in
    let s = connect a () s in
    let s = connect c bc_channel s in
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
       
  let t3 bc_channel sc =
    let s = sc in
    let s = connect b bc_channel s in
    let v,s = receive b s in
    Printf.printf "C: received: %s\n" v;
    let w,s = receive b s in
    Printf.printf "C: received: %s\n" w;
    close s


  let st =
    let cre () =
      {receiver=(fun (o,i) -> Channel.map i (fun v -> Marshal.from_bytes v 0));
       sender=(fun (o,i) v -> Channel.send o (Marshal.to_bytes v []))}
    in
    object
      method value=cre()
      method left=cre()
      method right=cre()
    end
    
  let main () =
    Random.self_init ();
    let g = global_example shmem shmem st in
    let sa, sb, sc = a#lens.get g, b#lens.get g, c#lens.get g in
    let (o1,i1), (o2,i2) = Channel.create (), Channel.create () in
    let t1id = Thread.create (fun () -> t1 (init ((),(),()) sa)) () in
    let t2id = Thread.create (fun () -> t2 (o1,i2) (init ((),(),()) sb)) () in
    t3 (o2,i1) (init ((),(),()) sc);
    Thread.join t1id;
    Thread.join t2id;
    ()
end

let () = Example1.main()
