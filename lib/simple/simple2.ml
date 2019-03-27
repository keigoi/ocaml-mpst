
type 'var receive = {recvfun:unit -> ([>] as 'var) Event.event}

type _ sess =
  | Send : (< .. > as 'obj) -> 'obj sess
  | Recv : ([>] as 'var) receive -> 'var receive sess
  | Close : unit sess

let unsess : type t. t sess -> t = function
  | Send(obj) -> obj
  | Recv(f) -> f
  | Close -> ()

let receive {recvfun} =
  Event.sync (recvfun ())

let close Close =
  ()

type ('la,'lb,'va,'vb) label =
    {make_obj: 'va -> 'la;
     make_var: 'vb -> 'lb}

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

type ('robj,'rvar,'c,'a,'b,'xs,'ys) role = {role:('robj,'rvar,'c,'c) label; lens:('a,'b,'xs,'ys) lens}

let (-->) a b label g0 =
  let ch = Event.new_channel () in
  let obj =
    b.role.make_obj @@
      label.make_obj
        (fun v ->
          Event.sync (Event.send ch v);
          unsess (lens_get a.lens g0)) in
  let g1 = lens_put a.lens g0 (Send obj) in
  let recvfun = fun () ->
    let ev = Event.receive ch in
    Event.wrap ev (fun v -> a.role.make_var @@ label.make_var (v, unsess (lens_get b.lens g1))) in
  let g2 = lens_put b.lens g1 (Recv {recvfun}) in
  g2

exception RoleNotEnabled
  
let merge_sess : type s. s sess -> s sess -> s sess = fun l r ->
  match l, r with
  | Send _, Send _ -> raise RoleNotEnabled
  | Recv f, Recv g -> Recv {recvfun=(fun () -> Event.choose [f.recvfun (); g.recvfun ()])}
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

let send_obj : 'obj. (< .. > as 'obj) sess -> 'obj = function
  | Send obj -> obj
  
let choice_at a merge (al,cl) (ar,cr) =
  let sal, sar = lens_get al.lens cl, lens_get ar.lens cr in
  let cl, cr = lens_put al.lens cl Close,
               lens_put ar.lens cr Close in
  let c = merge_slots cl cr in
  let lr = Send (merge.obj_merge (send_obj sal) (send_obj sar)) in
  lens_put a.lens c lr

let goto l =
  lazy (Lazy.force @@ Lazy.force l)

let a = {role={make_obj=(fun v->object method a=v end);
               make_var=(fun v->(`a(v):[`a of _]))}; (* explicit annotataion is mandatory *)
         lens=Fst}
let b = {role={make_obj=(fun v->object method b=v end);
               make_var=(fun v->(`b(v):[`b of _]))}; (* explicit annotataion is mandatory *)
         lens=Next Fst}
let c = {role={make_obj=(fun v->object method c=v end);
               make_var=(fun v->(`c(v):[`c of _]))}; (* explicit annotataion is mandatory *)
         lens=Next (Next Fst)}
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
let to_b m =
  {obj_merge=(fun l r -> object method b=m.obj_merge l#b r#b end)}
let to_b_or_c =
  {obj_merge=(fun l r -> object method b=l#b method c=r#c end)}
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
  let sa = unsess @@ lens_get a.lens g in
  let sb = unsess @@ lens_get b.lens g in
  let rec fA s =
    let s = s#b#msg () in
    fA s
  in
  let rec fB i s =
    if i>10 then raise Exit;
    let `a(`msg(_,s)) = receive s in
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

let create_g2 () =
  let rec loop =
    lazy begin
        choice_at a (to_b left_or_right)
        (a, (a --> b) left @@ goto loop)
        (a, (a --> b) right @@ goto loop)
      end
  in Lazy.force loop

let _ =
  let g2 = create_g2 () in
  let sa = unsess @@ lens_get a.lens g2 in
  let sb = unsess @@ lens_get b.lens g2 in
  let rec fA s =
    if Random.bool () then begin
        let s = s#b#left () in
        fA s
      end else begin
        let s = s#b#right () in
        fA s
      end
  in
  let rec fB i s =
    if i>10 then raise Exit;
    match receive s with
    | `a(`left(_, s)) ->
       print_endline "left";
       fB (i+1) s
    | `a(`right(_, s)) ->
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

(* let error () =
 *   let rec loop =
 *     lazy begin
 *         choice_at a to_b_or_c
 *         (a, (a --> b) right @@
 *             (b --> c) msg @@
 *             goto loop)
 *         (a, (a --> c) left @@
 *             (c --> b) msg @@ (* error: multiple choice subjects: A and C *)
 *             goto loop)
 *       end
 *   in Lazy.force loop *)

(* let _ =
 *   let g3 = create_g3 () in
 *   let sa = unsess @@ lens_get a.lens g3 in
 *   let sb = unsess @@ lens_get b.lens g3 in
 *   let sc = unsess @@ lens_get c.lens g3 in
 *   let rec fA s =
 *     if Random.bool () then begin
 *         print_endline "sending b-right@a";
 *         let s = s#b#right () in
 *         fA s
 *       end else begin
 *         print_endline "sending c-left@a";
 *         let s = s#c#left () in
 *         fA s
 *       end
 *   in
 *   let rec fB s =
 *     match receive s with
 *     | `a(`right(_, s)) ->
 *        print_endline "a-right@b\nsending c-msg@b";
 *        let s = s#c#msg () in
 *        fB s
 *     | `c(`msg(_, s)) ->
 *        print_endline "c-msg@b";
 *        fB s
 *   in
 *   let rec fC i s =
 *     if i>10 then raise Exit;
 *     match receive s with
 *     | `a(`left(_, s)) ->
 *        print_endline "a-left@c\nsending b-msg@c";
 *        let s = s#b#msg () in
 *        fC (i+1) s
 *     | `b(`msg(_, s)) ->
 *        print_endline "b-msg@c";
 *        fC (i+1) s
 *   in
 *   ignore @@ Thread.create fA sa;
 *   ignore @@ Thread.create fB sb;
 *   begin
 *     try
 *       ignore (fC 0 sc);
 *     with
 *       Exit -> print_endline "exit successfullly"
 *   end;
 *   () *)

let create_g3 () =
  let rec loop =
    lazy begin
        choice_at a to_b_or_c
        (a, (a --> b) left @@
            (a --> c) msg @@
            (b --> c) msg @@
            goto loop)
        (a, (a --> c) right @@
            (a --> b) msg @@
            (c --> b) msg @@
            goto loop)
      end
  in Lazy.force loop

let print_endline =
  let t = Mutex.create () in
  (fun str ->
    Mutex.lock t;
    print_endline str;
    Mutex.unlock t)

let _ =
  let g3 = create_g3 () in
  let sa = unsess @@ lens_get a.lens g3 in
  let sb = unsess @@ lens_get b.lens g3 in
  let sc = unsess @@ lens_get c.lens g3 in
  let rec fA s =
    if Random.bool () then begin
        print_endline "sending b-right@a";
        let s = s#b#left () in
        let s = s#c#msg () in
        fA s
      end else begin
        print_endline "sending c-left@a";
        let s = s#c#right () in
        let s = s#b#msg () in
        fA s
      end
  in
  let rec fB s =
    match receive s with
    | `a(`left(_, s)) ->
       print_endline "a-right@b\nsending c-msg@b";
       let s = s#c#msg () in
       fB s
    | `a(`msg(_, s)) ->
       print_endline "a-msg@b";
       let `c(`msg(_, s)) = receive s in
       fB s
  in
  let rec fC i s =
    if i>10 then raise Exit;
    match receive s with
    | `a(`right(_, s)) ->
       print_endline "a-left@c\nsending b-msg@c";
       let s = s#b#msg () in
       fC (i+1) s
    | `a(`msg(_, s)) ->
       let `b(`msg(_, s)) = receive s in
       print_endline "b-msg@c";
       fC (i+1) s
  in
  ignore @@ Thread.create fA sa;
  ignore @@ Thread.create fB sb;
  begin
    try
      ignore (fC 0 sc);
    with
      Exit -> print_endline "exit successfullly"
  end;
  ()
