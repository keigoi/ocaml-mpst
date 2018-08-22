include Base

module MPST = struct
  type 'a mpst = MPST of 'a list lazy_t

  exception RoleNotEnabled
  type 'k key = Key of 'k

  type ('v1, 'v2, 's1, 's2) lens = {get : 's1 -> 'v1; put : 's1 -> 'v2 -> 's2}
  type ('a, 'b, 'c, 'd, 'r, 'k) role = ('a, 'b, 'c, 'd) lens * 'r * 'k key

  type _ select = Select__
  type _ branch = Branch__
  type close = Close__

  type _ sess =
    | Select : 'a -> 'a select sess
    | SelectMulti : 'a -> 'a select sess
    | Branch : ('r1 * ('c -> 'a Lwt.t)) -> ('r1 * ('c -> 'a)) branch sess (* "fancy" type rep. (hiding Lwt.t) *)
    | DummyBranch : 'a branch sess
    | Close : close sess

  type (_, _, _, _) dlabel = Dlabel :{slabel:'d -> 'k1 -> 'd1; rlabel:'f -> 'k2 -> 'f1 Lwt.t} -> ('k1 -> 'd1, 'd, 'k2 -> 'f1, 'f) dlabel  (* hiding Lwt.t*)

  type (_,_,_,_,_,_) dlabel2 = Dlabel2 : {sender2 : 'x1 * 'x2 -> 'k1 -> 'l; receiver2: ('y1 * 'y2) -> 'k2 -> 'r Lwt.t} -> ('k1 -> 'l, 'x1, 'x2, 'k2 -> 'r, 'y1, 'y2) dlabel2

  let msg (type k1 k2 v g h) push st : (k1 -> <msg : v -> g sess>, g sess, k2 -> [`msg of v * h sess], h sess) dlabel =
    Dlabel {slabel=(fun x c -> object method msg v=(push v:unit);x end); rlabel=(fun x c -> Lwt.bind (st c) @@ fun v -> Lwt.return (`msg(v,x)))}

  let flatten : 'a mpst -> 'a mpst -> 'a mpst = fun (MPST m1) (MPST m2) ->
    MPST (lazy (Lazy.force m1 @ Lazy.force m2))

  let (-->) : type d1 f1 d f s t u v r1 r2 c k1 k2.
       (d sess, (r2 * d1) select sess, s mpst, t mpst, r1, k1) role
    -> (f sess, (r1 * (k2 -> f1)) branch sess, t mpst, u mpst, r2, k2) role
    -> (unit -> (d1, d sess, k2 -> f1, f sess) dlabel)
    -> s mpst
    -> u mpst =
    fun (r1_l, r1, k1) (r2_l, r2, k2) dlabf sobj ->
      let Dlabel dlab = dlabf () in
      let d = lazy (r1_l.get sobj) in
      let tobj = r1_l.put sobj (Select (r2, fun c -> dlab.slabel (Lazy.force d) c)) in
      let f = lazy (r2_l.get tobj) in
      let uobj = r2_l.put tobj (Branch (r1, fun c -> dlab.rlabel (Lazy.force f) c)) in
      uobj

  let leftright push st = Dlabel2
                            {sender2=(fun (x,y) c->object method left v=push (Left v);x method right v=push (Right y);y end);
                   receiver2=(fun (x,y) c-> Lwt.map (function Left v -> `left(v,x) | Right v -> `right(v,x)) (st c))}

  let (-!%%->) : type r1 r2 d1 d2 f1 f2 ss s1 s2 t t1 t2 u v1 v2 r l k1 k2.
       (close sess, (r2 * (k1 -> l)) select sess, ss mpst, t mpst, r1, k1) role
    -> (close sess, (r1 * (k2 -> r)) branch sess, t mpst, u mpst, r2, k2) role
    -> (unit -> (k1 -> l, d1 sess, d2 sess, k2 -> r, f1 sess, f2 sess) dlabel2)
    -> l1:(((d1 sess, close sess, s1 mpst, t1 mpst, r1, k1) role * (f1 sess, close sess, t1 mpst, ss mpst, r2, k2) role) * s1 mpst)
    -> l2:(((d2 sess, close sess, s2 mpst, t2 mpst, r1, k1) role * (f2 sess, close sess, t2 mpst, ss mpst, r2, k2) role) * s2 mpst)
    -> u mpst =
    fun (r1_l, r1, _) (r2_l, r2, _) dlab
        ~l1:(((r1_ll,_,_),(r2_ll,_,_)), s1obj)
        ~l2:(((r1_lr,_,_),(r2_lr,_,_)), s2obj) ->
      let Dlabel2 dlab = dlab () in
      let d1,t1obj = lazy (r1_ll.get s1obj), r1_ll.put s1obj Close in
      let f1,ssobj_l = lazy (r2_ll.get t1obj), r2_ll.put t1obj Close in
      let d2,t2obj = lazy (r1_lr.get s2obj), r1_lr.put s2obj Close in
      let f2,ssobj_r = lazy (r2_lr.get t2obj), r2_lr.put t2obj Close in
      let dd = SelectMulti (r2, fun c -> dlab.sender2 (Lazy.force d1, Lazy.force d2) c) in
      let ff = Branch (r1, fun c -> dlab.receiver2 (Lazy.force f1, Lazy.force f2) c) in
      let tobj_l = r1_l.put ssobj_l dd in
      let uobj_l = r2_l.put tobj_l ff in
      let tobj_r = r1_l.put ssobj_r dd in
      let uobj_r = r2_l.put tobj_r ff in
      flatten uobj_l uobj_r

  (* let (-%%->) r1 r2 ~left ~right  = (-!%%->) r1 r2 leftright ~l1:left ~l2:right *)

  let dummy_close : 'a 'r 'k. (close sess, close sess, 'a mpst, 'a mpst, 'r, 'k) role -> 'a mpst -> 'a mpst =
    fun (l,_,_) aobj ->
    l.put aobj Close

  let dummy_receive : 'a 'l 'r 'k. ('l branch sess, 'l branch sess, 'a mpst, 'a mpst, 'r1, 'k) role -> 'a mpst -> 'a mpst =
    fun (l,_,_) aobj ->
    l.put aobj DummyBranch (* never fires *)

  let finish =
    MPST (Lazy.from_val [object method a=Close method b=Close method c=Close end])

  let unmpst (MPST (lazy m)) = m

  let loop_ (m : 'a mpst lazy_t) =
    MPST (lazy (unmpst (Lazy.force m)))

  let get_sess : 's 'a. ('s sess, _, 'a mpst, _, _, _) role -> 'a mpst -> 's sess = fun (l,_,_) s -> l.get s

  let send : 'r 'l 'v 's 'k. 'r -> ((< .. > as 'l) -> 'v -> 's sess) -> 'v -> ('r * ('k -> 'l)) select sess -> 's sess =
    fun _ g v (Select (_,r)|SelectMulti (_,r)) ->
    g (r (failwith "")) v

  let receive : 'r 'l 'k. 'r -> ('r * ('k -> 'l)) branch sess -> 'l Lwt.t =
    fun _ s -> match s with
               | Branch (_,l) -> l (failwith "")
               | DummyBranch -> failwith "dummy branch encountered"

  let close : close sess -> unit = fun _ -> ()

  let rec unify : type s. s sess -> s sess -> s sess = fun s1 s2 ->
    begin
      match s1, s2 with
      | Branch (r1,a1), Branch (r2,a2) -> if a1==a2 then Branch (r1,a1) else Branch (r1, fun k -> Lwt.choose [a1 k; a2 k])
      | b, DummyBranch -> b
      | DummyBranch, b -> b
      | Close, Close -> Close
      | (SelectMulti _) as x, SelectMulti _ -> x (* FIXME: must raise an exception in some cases -- *)
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

  let a : 'a1 'a2 'b 'c. ('a1 sess, 'a2 sess, <a:'a1 sess; b:'b sess; c:'c sess> mpst, <a:'a2 sess; b:'b sess; c:'c sess> mpst, a, _) role =
    {get=(fun s -> uget ObjLens.a s); put=(fun s v -> MPST (lazy [object method a=v method b=uget ObjLens.b s method c=uget ObjLens.c s end]))}, A, Key ()
  let b =
    {get=(fun s -> uget ObjLens.b s); put=(fun s v -> MPST (lazy [object method a=uget ObjLens.a s method b=v method c=uget ObjLens.c s end]))}, B, ()
  let c =
    {get=(fun s -> uget ObjLens.c s); put=(fun s v -> MPST (lazy [object method a=uget ObjLens.a s method b=uget ObjLens.b s method c=v end]))}, C, ()
end
