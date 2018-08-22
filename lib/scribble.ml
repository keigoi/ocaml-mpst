include Base

module MPST = struct
  type 'a mpst = MPST of 'a list lazy_t

  exception RoleNotEnabled

  type ('v1, 'v2, 's1, 's2) lens = {get : 's1 -> 'v1; put : 's1 -> 'v2 -> 's2}
  type ('a, 'b, 'c, 'd, 'r) role = ('a, 'b, 'c, 'd) lens * 'r
  type ('d1, 'd, 'f1, 'f) dlabel = {slabel:'d -> 'd1; rlabel:'f -> 'f1}

  type _ select = Select__
  type _ branch = Branch__
  type close = Close__

  let msg : (<msg : 'g>, 'g, [`msg of 'h], 'h) dlabel =
    {slabel=(fun x -> object method msg=x end); rlabel=(fun x -> `msg(x))}

  let left : (<left: 'g>, 'g, [>`left of 'h], 'h) dlabel =
    {slabel=(fun x -> object method left=x end); rlabel=(fun x -> `left(x))}

  let right : (<right: 'g>, 'g, [>`right of 'h], 'h) dlabel =
    {slabel=(fun x -> object method right=x end); rlabel=(fun x -> `right(x))}

  let middle : (<middle: 'g>, 'g, [>`middle of 'h], 'h) dlabel =
    {slabel=(fun x -> object method middle=x end); rlabel=(fun x -> `middle(x))}

  let lab1 : (<lab1: 'g>, 'g, [>`lab1 of 'h], 'h) dlabel =
    {slabel=(fun x -> object method lab1=x end); rlabel=(fun x -> `lab1(x))}

  let lab2 : (<lab2: 'g>, 'g, [>`lab2 of 'h], 'h) dlabel =
    {slabel=(fun x -> object method lab2=x end); rlabel=(fun x -> `lab2(x))}

  let lab3 : (<lab3: 'g>, 'g, [>`labe of 'h], 'h) dlabel =
    {slabel=(fun x -> object method lab3=x end); rlabel=(fun x -> `lab3(x))}

  type _ sess =
    | Select : 'a -> 'a select sess
    | SelectMulti : 'a -> 'a select sess
    | Branch : (unit -> 'a Lwt.t) -> 'a branch sess
    | DummyBranch : 'a branch sess
    | Close : close sess

  let flatten : 'a mpst -> 'a mpst -> 'a mpst = fun (MPST m1) (MPST m2) ->
    MPST (lazy (Lazy.force m1 @ Lazy.force m2))


  let (-->) : type d1 f1 d f s t u v r1 r2.
       (d sess, (r2 * d1) select sess, s mpst, t mpst, r1) role
    -> (f sess, (r1 * f1) branch sess, t mpst, u mpst, r2) role
    -> (d1, v -> d sess, f1, v * f sess) dlabel
    -> s mpst
    -> u mpst =
    fun (r1_l, r1) (r2_l, r2) dlab sobj ->
      let st, push = Lwt_stream.create () in
      let d = lazy (r1_l.get sobj) in
      let tobj = r1_l.put sobj (Select (r2, dlab.slabel (fun v -> push (Some v); Lazy.force d))) in
      let f = lazy (r2_l.get tobj) in
      let uobj = r2_l.put tobj (Branch (fun () -> Lwt.map (fun v -> r1, dlab.rlabel (v, Lazy.force f)) (Lwt_stream.next st))) in
      uobj

  type ('l, 'x1, 'x2, 'r, 'y1, 'y2) dlabel2 = {sender2 : 'x1 * 'x2 -> 'l; receiver2: ('y1, 'y2) either -> 'r}

  let leftright = {sender2=(fun (x,y)->object method left=x method right=y end);
                   receiver2=(function Left x -> `left x | Right x -> `right x)}

  let (-!%%->) : type r1 r2 d1 d2 f1 f2 ss s1 s2 t t1 t2 u v1 v2 r l.
       (close sess, (r2 * l) select sess, ss mpst, t mpst, r1) role
    -> (close sess, (r1 * r) branch sess, t mpst, u mpst, r2) role
    -> (l, v1 -> d1 sess, v2 -> d2 sess, r, v1 * f1 sess, v2 * f2 sess) dlabel2
    -> l1:(((d1 sess, close sess, s1 mpst, t1 mpst, r1) role * (f1 sess, close sess, t1 mpst, ss mpst, r2) role) * s1 mpst)
    -> l2:(((d2 sess, close sess, s2 mpst, t2 mpst, r1) role * (f2 sess, close sess, t2 mpst, ss mpst, r2) role) * s2 mpst)
    -> u mpst =
    fun (r1_l, r1) (r2_l, r2) dlab
        ~l1:(((r1_ll,_),(r2_ll,_)), s1obj)
        ~l2:(((r1_lr,_),(r2_lr,_)), s2obj) ->
      let d1,t1obj = lazy (r1_ll.get s1obj), r1_ll.put s1obj Close in
      let f1,ssobj_l = lazy (r2_ll.get t1obj), r2_ll.put t1obj Close in
      let d2,t2obj = lazy (r1_lr.get s2obj), r1_lr.put s2obj Close in
      let f2,ssobj_r = lazy (r2_lr.get t2obj), r2_lr.put t2obj Close in
      let st, push = Lwt_stream.create () in
      let dd = SelectMulti
                 (r2, dlab.sender2
                        ((fun v ->
                          push (Some (Left v)); Lazy.force d1),
                         (fun v ->
                          push (Some (Right  v)); Lazy.force d2)))
      in
      let ff =  Branch
                  (fun () -> Lwt.map (function
                       | Left v -> r1, dlab.receiver2 (Left (v, Lazy.force f1))
                       | Right v -> r1, dlab.receiver2 (Right (v, Lazy.force f2))) (Lwt_stream.next st))
      in
      let tobj_l = r1_l.put ssobj_l dd in
      let uobj_l = r2_l.put tobj_l ff in
      let tobj_r = r1_l.put ssobj_r dd in
      let uobj_r = r2_l.put tobj_r ff in
      flatten uobj_l uobj_r

  let (-%%->) r1 r2 ~left ~right  = (-!%%->) r1 r2 leftright ~l1:left ~l2:right

  let dummy_close : 'a 'r. (close sess, close sess, 'a mpst, 'a mpst, 'r) role -> 'a mpst -> 'a mpst =
    fun (l,_) aobj ->
    l.put aobj Close

  let dummy_receive : 'a 'l 'r. ('l branch sess, 'l branch sess, 'a mpst, 'a mpst, 'r1) role -> 'a mpst -> 'a mpst =
    fun (l,_) aobj ->
    l.put aobj DummyBranch (* never fires *)

  let finish =
    MPST (Lazy.from_val [object method a=Close method b=Close method c=Close end])

  let unmpst (MPST (lazy m)) = m

  let loop_ (m : 'a mpst lazy_t) =
    MPST (lazy (unmpst (Lazy.force m)))

  let get_sess : 's 'a. ('s sess, _, 'a mpst, _, _) role -> 'a mpst -> 's sess = fun (l,_) s -> l.get s

  let send : 'r 'l 'v 's. 'r -> ((< .. > as 'l) -> 'v -> 's sess) -> 'v -> ('r * 'l) select sess -> 's sess =
    fun _ g v (Select (_,r)|SelectMulti (_,r)) ->
    g r v

  let receive : 'r 'l. 'r -> ('r * 'l) branch sess -> 'l Lwt.t =
    fun _ s -> match s with
               | Branch l -> Lwt.map snd (l ())
               | DummyBranch -> failwith "dummy branch encountered"

  let close : close sess -> unit = fun _ -> ()

  let rec unify : type s. s sess -> s sess -> s sess = fun s1 s2 ->
    begin
      match s1, s2 with
      | Branch a1, Branch a2 -> if a1==a2 then Branch a1 else Branch (fun () -> Lwt.choose [a1 (); a2 ()])
      | b, DummyBranch -> b
      | DummyBranch, b -> b
      | Close, Close -> Close
      | (SelectMulti _) as x, SelectMulti _ -> x (* FIXME: must raise exception in some cases -- *)
      | Select _, _ -> raise RoleNotEnabled
      | _, Select _ -> raise RoleNotEnabled
    end

  module ObjLens = struct
    let a : 'a1 'a2 'b 'c. ('a1, 'a2, <a:'a1; b:'b; c:'c>, <a:'a2; b:'b; c:'c>) lens =
      {get=(fun s -> s#a); put=(fun s v -> object method a=v method b=s#b method c=s#c end)}
    let b : 'a 'b1 'b2 'c. ('b1, 'b2, <a:'a; b:'b1; c:'c>, <a:'a; b:'b2; c:'c>) lens =
      {get=(fun s -> s#b); put=(fun s v -> object method a=s#a method b=v method c=s#c end)}
    let c : 'a 'b 'c1 'c2. ('c1, 'c2, <a:'a; b:'b; c:'c1>, <a:'a; b:'b; c:'c2>) lens =
      {get=(fun s -> s#c); put=(fun s v -> object method a=s#a method b=s#b method c=v end)}
  end

  type a = A
  type b = B
  type c = C

  let rec uget : 's 'a. ('s sess, _, 'a, _) lens -> 'a mpst -> 's sess = fun l (MPST (lazy xs)) ->
    let x = List.fold_left unify (l.get (List.hd xs)) (List.map l.get (List.tl xs)) in
    x

  let a : 'a1 'a2 'b 'c. ('a1 sess, 'a2 sess, <a:'a1 sess; b:'b sess; c:'c sess> mpst, <a:'a2 sess; b:'b sess; c:'c sess> mpst, a) role =
    {get=(fun s -> uget ObjLens.a s); put=(fun s v -> MPST (lazy [object method a=v method b=uget ObjLens.b s method c=uget ObjLens.c s end]))}, A
  let b =
    {get=(fun s -> uget ObjLens.b s); put=(fun s v -> MPST (lazy [object method a=uget ObjLens.a s method b=v method c=uget ObjLens.c s end]))}, B
  let c =
    {get=(fun s -> uget ObjLens.c s); put=(fun s v -> MPST (lazy [object method a=uget ObjLens.a s method b=uget ObjLens.b s method c=v end]))}, C
end
