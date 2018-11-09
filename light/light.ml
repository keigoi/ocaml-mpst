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
  let choose xs =
    Event.sync (Event.choose (List.map (fun (t,a) -> Event.wrap (Event.receive t) (fun v -> v,a)) xs))
end
    

(* module Local = struct *)

  type ('a,'b) either = Left of 'a | Right of 'b

  type ('v1,'v2,'s1,'s2) lens = {get : 's1 -> 'v1; put : 's1 -> 'v2 -> 's2}
  type ('r,'v1,'v2,'s1,'s2) role = {role:'r; lens:('v1,'v2,'s1,'s2) lens; merge: 's2 -> 's2 -> 's2}

  type ('c,'v,'r) send = Send of 'r * 'v Channel.t * 'c list
  type ('c,'v,'r) recv = Recv of 'r * 'v Channel.t * 'c list

  type ('c1,'c2,'r) select = Select of 'r * bool Channel.t * 'c1 list * 'c2 list
  type ('c1,'c2,'r) branch = Branch of 'r * bool Channel.t * 'c1 list * 'c2 list

  type close = Close

  type 'c sess = Sess of 'c list

  let send (_:'r) v (Sess s) =
    match s with
    | [Send ((_:'r),ch,cont)] ->
       Channel.send ch v;
       Sess cont
    | _ -> failwith "role not enabled"
          
  let receive (_:'r) (Sess s) =
    let xs = List.map (fun (Recv ((_:'r), ch, c)) -> ch, c) s in
    let v,cont = Channel.choose xs in
    v, Sess cont

  let _select b (_:'r) (Sess s) =
    match s with
    | [Select ((_:'r),ch,ls,rs)] ->
       Channel.send ch b;
       (ls,rs)
    | _ -> failwith "role not enabled"

  let select_left r s = Sess (fst (_select true r s))
  let select_right r s = Sess (snd (_select false r s))

  let branch (_:'r) (Sess s) =
    let xs = List.map (fun (Branch ((_:'r),ch,ls,rs)) -> (ch,(ls,rs))) s in
    let b, (ls,rs) = Channel.choose xs in
    if b then
      Left (Sess ls)
    else
      Right (Sess rs)

  let close (Sess (_:close list)) = ()
(* end
 * 
 * module Global = struct
 *   open Local *)
     

  let (-->) a b s =
    let ch = Channel.create () in
    let (Sess sa) = a.lens.get s in
    let s = a.lens.put s (Sess [Send (b.role, ch, sa)]) in
    let (Sess sb) = b.lens.get s in
    let s = b.lens.put s (Sess [Recv (a.role, ch, sb)]) in
    s

  let (-%%->) a b ((al,bl),sl) ((ar,br),sr) =
    let ch = Channel.create () in
    let Sess sal, Sess sar = al.lens.get sl, ar.lens.get sr in
    let sl, sr = al.lens.put sl (Sess [Close]), ar.lens.put sr (Sess [Close]) in
    let Sess sbl, Sess sbr = bl.lens.get sl, br.lens.get sr in
    let sl, sr = bl.lens.put sl (Sess [Close]), br.lens.put sr (Sess [Close]) in
    let s = bl.merge sl sr in
    let s = a.lens.put s (Sess [Select (b.role, ch, sal, sar)]) in
    let s = b.lens.put s (Sess [Branch (a.role, ch, sbl, sbr)]) in
    s
(* end
 * 
 * module ThreeParty = struct
 *   open Local *)
  let finish3 =
    let (s1,s2,s3) as roles = [Close],[Close],[Close] in
    Sess s1, Sess s2, Sess s3

  type a = A
  type b = B
  type c = C

  let merge3 (Sess xs,Sess ys,Sess zs) (Sess xs',Sess ys',Sess zs') = Sess (xs@xs'),Sess (ys@ys'),Sess (zs@zs')

  let a = {role=A; lens={get=(fun (x,_,_) -> x); put=(fun (_,y,z) x->(x,y,z))}; merge=merge3}
  let b = {role=B; lens={get=(fun (_,y,_) -> y); put=(fun (x,_,z) y->(x,y,z))}; merge=merge3}
  let c = {role=C; lens={get=(fun (_,_,z) -> z); put=(fun (x,y,_) z->(x,y,z))}; merge=merge3}
(* end *)

(* open ThreeParty *)

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

(* open Local *)
   
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
