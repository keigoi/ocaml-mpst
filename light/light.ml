(* dune build light/light.exe && ./_build/default/light/light.exe *)

module Channel : sig
  type 'a t
  val create : unit -> 'a t
  val send : 'a t -> 'a -> unit
  val recv : 'a t -> 'a
  val choose : ('a t * 'b) list -> 'a * 'b
end = struct
  type 'a t = 'a Event.channel
  let create () = Event.new_channel ()
  let send t v = Event.sync (Event.send t v)
  let recv t = Event.sync (Event.receive t)
  let wrap (t,a) =
    Event.wrap (Event.receive t) (fun v -> v, a)
  let choose xs =
    Event.sync (Event.choose (List.map wrap xs))
end
    
module UnsafeChannel : sig
  type t
  val create : unit -> t
  val send : t -> 'a -> unit
  val recv : t -> 'a
  val choose : (t * 'b) list -> 'a * 'b
end = struct
  type t = unit Event.channel
  let create () = Event.new_channel ()
  let send t v = Event.sync (Event.send t (Obj.magic v))
  let recv t = Obj.magic (Event.sync (Event.receive t))
  let wrap (t,a) =
    Event.wrap (Event.receive t) (fun v -> Obj.magic v, a)
  let choose xs =
    Event.sync (Event.choose (List.map wrap xs))
end

module Local : sig
  type ('a, 'b) either = Left of 'a | Right of 'b
  type ('c, 'v, 'r) send
  type ('c, 'v, 'r) recv
  type ('c1, 'c2, 'r) select
  type ('c1, 'c2, 'r) branch
  type close
  type 'c sess
  val send : 'r -> 'v -> ('c,'v,'r) send sess -> 'c sess
  val receive : 'r -> ('c,'v,'r) recv sess -> 'v * 'c sess
  val select_left : 'r -> ('c1,'c2,'r) select sess -> 'c1 sess
  val select_right : 'r -> ('c1,'c2,'r) select sess -> 'c2 sess
  val branch : 'r -> ('c1,'c2,'r) branch sess -> ('c1 sess, 'c2 sess) either
  val close : 'a sess -> unit

  (* session creation *)
  module Internal : sig
    val s2c : from:('r1 * 's1 sess Lazy.t) ->
              to_:('r2 * 's2 sess Lazy.t) ->
              ('s1, 'v, 'r2) send sess * ('s2, 'v, 'r1) recv sess
    val s2c_branch :
      from:('r1 * ('sl1 sess Lazy.t * 'sr1 sess Lazy.t)) ->
      to_:('r2 * ('sl2 sess Lazy.t * 'sr2 sess Lazy.t)) ->
      ('sl1, 'sr1, 'r2) select sess * ('sl2, 'sr2, 'r1) branch sess
    val end_ : close sess

    (* merge operator for global description *)
    val merge : 'a sess -> 'a sess -> 'a sess
  end

end = struct

  type ('a,'b) either = Left of 'a | Right of 'b

  type ('c,'v,'r) send
  type ('c,'v,'r) recv

  type ('c1,'c2,'r) select
  type ('c1,'c2,'r) branch

  type close

  type raw_sess =
    Send of UnsafeChannel.t * raw_sess Lazy.t
  | Recv of (UnsafeChannel.t * raw_sess Lazy.t) list
  | Select of bool Channel.t * raw_sess Lazy.t * raw_sess Lazy.t
  | Branch of (bool Channel.t * (raw_sess Lazy.t * raw_sess Lazy.t)) list
  | Close

  type 'c sess = Sess of raw_sess
  let unsess (Sess s) = s

  let send _ v (Sess s) =
    match s with
    | Send (ch,cont) ->
       UnsafeChannel.send ch v;
       Sess (Lazy.force cont)
    | _ -> assert false
          
  let receive (_:'r) (Sess s) =
    match s with
    | Recv xs ->
       let v,s = UnsafeChannel.choose xs in
       v, Sess (Lazy.force s)
    | _ -> assert false

  let _select b (Sess s) =
    match s with
    | Select (ch,ls,rs) ->
       Channel.send ch b;
       (ls,rs)
    | _ -> assert false

  let select_left r s = Sess (Lazy.force (fst (_select true s)))
  let select_right r s = Sess (Lazy.force (snd (_select false s)))

  let branch (_:'r) (Sess s) =
    match s with
    | Branch (xs) ->
       let b, (ls, rs) = Channel.choose xs in
       if b then
         Left (Sess (Lazy.force ls))
       else
         Right (Sess (Lazy.force rs))
    | _ -> assert false

  let close (Sess _) = ()

  let unsess' s = lazy (unsess (Lazy.force s))

  module Internal = struct
    
    let s2c ~from:(_,s1) ~to_:(_,s2) =
      let ch = UnsafeChannel.create () in
      Sess (Send (ch, unsess' s1)), Sess (Recv [(ch, unsess' s2)])

    let s2c_branch ~from:(_,(sl1,sr1)) ~to_:(_,(sl2,sr2)) =
      let ch = Channel.create () in
      Sess (Select (ch, unsess' sl1, unsess' sr1)),
      Sess (Branch [(ch, (unsess' sl2, unsess' sr2))])

    let end_ = Sess Close

    let merge (Sess x) (Sess y) =
      match x, y with
      | Send (_,_), Send (_,_)
        | Select (_,_,_), Select (_,_,_) ->
         failwith "role not enabled"
      | Recv xs, Recv ys ->
         Sess (Recv (xs @ ys))
      | Branch xs, Branch ys ->
         Sess (Branch (xs @ ys))
      | Close, Close ->
         Sess Close
      | Send (_,_), _ | Recv _, _ | Select (_,_,_), _ | Branch _, _ | Close, _
        -> assert false
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
                                     
  open Local
  val ( --> ) :
    ('r1, 's1 sess, ('s1, 'v, 'r2) send sess, 'a0, 'a1) role ->
    ('r2, 's2 sess, ('s2, 'v, 'r1) recv sess, 'a1, 'a2) role ->
    'a0 -> 'a2

  val ( -%%-> ) :
    ('r1, close sess, ('sl1, 'sr1, 'r2) select sess, 'a2, 'a3) role ->
    ('r2, close sess, ('sl2, 'sr2, 'r1) branch sess, 'a3, 'a4) role ->
    (('r1, 'sl1 sess, close sess, 'al0, 'al1) role *
     ('r2, 'sl2 sess, close sess, 'al1, 'a2) role) *
    'al0 ->
    (('r1, 'sr1 sess, close sess, 'ar0, 'ar1) role *
     ('r2, 'sr2 sess, close sess, 'ar1, 'a2) role) *
    'ar0 -> 'a4
    
  val choice : ('r1, close, 's sess, 'a1, 'a2) role ->
    ('sl sess -> 'sr sess -> 's sess) ->
    ('r1, 'sl sess, close sess, 'al0, 'a1) role * 'al0 ->
    ('r1, 'sr sess, close sess, 'ar0, 'a1) role * 'ar0 ->
    'a2
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
  open Local.Internal

  let (-->) a b s =
    let sa = lazy (a.lens.get s) in
    let rec sb = lazy (b.lens.get (Lazy.force s')) 
    and sab' = lazy (s2c ~from:(a.role, sa) ~to_:(b.role, sb))
    and s' = lazy (a.lens.put s (fst (Lazy.force sab'))) in
    let s'' = b.lens.put (Lazy.force s') (snd (Lazy.force sab')) in
    s''

  let choice a merge (al,sl) (ar,sr) =
    let sal, sar = al.lens.get sl, ar.lens.get sr in
    let sl, sr = al.lens.put sl end_, ar.lens.put sr end_ in
    let s = al.merge sl sr in
    a.lens.put s (merge sal sar)

  let (-%%->) a b ((al,bl),sl) ((ar,br),sr) =
    let sal, sar = lazy (al.lens.get sl), lazy (ar.lens.get sr) in
    let sl, sr = al.lens.put sl end_, ar.lens.put sr end_ in
    let sbl, sbr = lazy (bl.lens.get sl), lazy (br.lens.get sr) in
    let sl, sr = bl.lens.put sl end_, br.lens.put sr end_ in
    let s = bl.merge sl sr in
    let sa, sb = s2c_branch ~from:(a.role, (sal,sar)) ~to_:(b.role, (sbl,sbr)) in
    let s = a.lens.put s sa in
    let s = b.lens.put s sb in
    s
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

let global_example () =
  (* let open Global in *)
  (a --> b) @@
  (b --> c) @@
  (a -%%-> b)
    ((a,b), b --> c @@
            finish3)
    ((a,b), b --> a @@
            b --> c @@
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
  match branch A s with
  | Left s ->
     let s = send C "to C (1)" s in
     close s
  | Right s ->
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

let () = main ()
