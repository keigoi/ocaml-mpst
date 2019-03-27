type ('r, 'obj) send = Send__
type ('r, 'var) receive = Recv__
type close = Close__

type _ sess =
  | Send : (([>] as 'r) * (< .. > as 'obj)) -> ('r, 'obj) send sess
  | Recv : (([>] as 'r) * (unit -> ([>] as 'var) Event.event)) -> ('r, 'var) receive sess
  | Close : close sess

let send (_:'r) sel v (Send((_:'r), obj)) =
  sel obj v

let receive (_:'r) (Recv((_:'r), recvfun)) =
  Event.sync (recvfun ())

let close Close =
  ()

type ('la,'lb,'ca,'cb,'v) label =
    {make_obj: ('v -> 'ca) -> 'la;
     make_var: 'v * 'cb -> 'lb}

type _ slots =
  Cons : 'x sess * 'xs slots lazy_t -> ('x * 'xs) slots
| Nil : unit slots

type (_,_,_,_) lens =
  | Fst  : ('a, 'b, ('a * 'xs) slots, ('b * 'xs) slots) lens
  | Next : ('a,'b, 'xs slots,'ys slots) lens
           -> ('a,'b, ('x * 'xs) slots, ('x * 'ys) slots) lens
            

let slot_head : type hd tl. (hd * tl) slots lazy_t -> hd sess = fun sl ->
  match sl with
  | lazy (Cons(hd,_)) -> hd

let slot_tail : type hd tl. (hd * tl) slots lazy_t -> tl slots lazy_t = fun sl ->
  lazy begin
      match sl with
      | lazy (Cons(_,lazy tl)) -> tl
    end
  
let rec lens_get : type a b xs ys. (a, b, xs, ys) lens -> xs lazy_t -> a sess = fun ln xs ->
  match ln with
  | Fst -> slot_head xs
  | Next ln' -> lens_get ln' (slot_tail xs)

let rec lens_put : type a b xs ys. (a,b,xs,ys) lens -> xs lazy_t -> b sess -> ys lazy_t =
  fun ln xs b ->
  match ln with
  | Fst -> lazy (Cons(b, slot_tail xs))
  | Next ln' ->
     lazy
       begin match xs with
       | lazy (Cons(a, xs')) -> Cons(a, lens_put ln' xs' b)
       end

type ('r,'a,'b,'xs,'ys) role = {role:'r; lens:('a,'b,'xs,'ys) lens}

let (-->) a b mklabel g0 =
  let ch = Event.new_channel () in
  let obj =
    mklabel.make_obj
      (fun v ->
        Event.sync (Event.send ch v);
        lens_get a.lens g0) in
  let g1 = lens_put a.lens g0 (Send (b.role, obj)) in
  let recvfun = fun () ->
    let ev = Event.receive ch in
    Event.wrap ev (fun v -> mklabel.make_var (v, lens_get b.lens g1)) in
  let g2 = lens_put b.lens g1 (Recv (a.role, recvfun)) in
  g2

exception RoleNotEnabled
  
let merge_sess : type s. s sess -> s sess -> s sess = fun l r ->
  match l, r with
  | Send (_, _), Send (_, _) -> raise RoleNotEnabled
  | Recv (r, f), Recv (_, g) -> Recv(r, (fun () -> Event.choose [f (); g ()]))
  | Close, Close -> Close
                    
  
let rec merge_slots : type t. t slots lazy_t -> t slots lazy_t -> t slots lazy_t = fun ls rs ->
  lazy begin
      match ls, rs with
      | lazy (Cons(hd_l,tl_l)), lazy (Cons(hd_r, tl_r)) ->
         (Cons(merge_sess hd_l hd_r, merge_slots tl_l tl_r))
      | lazy Nil, _ ->
         Nil
    end

type ('l, 'r, 'lr) obj_merge =
    {obj_merge: 'l -> 'r -> 'lr}

let send_role : type r obj. (r, obj) send sess -> r = function
  | Send (r, _) -> r

let send_obj : type r obj. (r, obj) send sess -> obj = function
  | Send (_, obj) -> obj
  
let choice_at a m (al,cl) (ar,cr) =
  let sal, sar = lens_get al.lens cl, lens_get ar.lens cr in
  let cl, cr = lens_put al.lens cl Close,
               lens_put ar.lens cr Close in
  let c = merge_slots cl cr in
  let lr = Send (send_role sar, m.obj_merge (send_obj sal) (send_obj sar)) in
  lens_put a.lens c lr

let goto l =
  lazy (Lazy.force @@ Lazy.force l)

let a = {role=(`A:[`A]); lens=Fst}
let b = {role=(`B:[`B]); lens=Next Fst}
let msg =
  {make_obj=(fun f -> object method msg=f end);
   make_var=(fun v -> `msg(v))}
let left =
  {make_obj=(fun f -> object method left=f end);
   make_var=(fun v -> `left(v))}
let right =
  {make_obj=(fun f -> object method right=f end);
   make_var=(fun v -> `right(v))}
let left_or_right =
  {obj_merge=(fun l r -> object method left=l#left method right=r#right end)}
  
let finish = Lazy.from_val @@ Cons(Close,Lazy.from_val @@ Cons(Close,Lazy.from_val Nil))

let create_g () =
  let rec loop =
    lazy begin
        (a --> b) msg @@
        goto loop
      end
  in Lazy.force loop

exception Exit

let _ =
  let g = create_g () in
  let sa = lens_get a.lens g in
  let sb = lens_get b.lens g in
  let rec fA s =
    let s = send `B (fun x->x#msg) () s in
    fA s
  in
  let rec fB i s =
    if i>10 then raise Exit;
    let `msg(_,s) = receive `A s in
    Printf.printf "%d\n" i;
    fB (i+1) s
  in
  ignore @@ Thread.create fA sa;
  begin
    try
      ignore (fB 0 sb);
    with
      Exit -> print_endline "exit successfully";
  end;
  ()

let create_h () =
  let rec loop =
    lazy begin
        choice_at a left_or_right
        (a, (a --> b) left @@ goto loop)
        (a, (a --> b) right @@ goto loop)
      end
  in Lazy.force loop

let _ =
  let h = create_h () in
  let sa = lens_get a.lens h in
  let sb = lens_get b.lens h in
  let rec fA s =
    if Random.bool () then begin
        let s = send `B (fun x->x#left) () s in
        fA s
      end else begin
        let s = send `B (fun x->x#right) () s in
        fA s
      end
  in
  let rec fB i s =
    if i>10 then raise Exit;
    match receive `A s with
    | `left(_, s) ->
       print_endline "left";
       fB (i+1) s
    | `right(_, s) ->
       print_endline "right";
       fB (i+1) s
  in
  ignore @@ Thread.create fA sa;
  begin
    try
      ignore (fB 0 sb);
    with
      Exit -> print_endline "exit successfullly"
  end;
  ()
