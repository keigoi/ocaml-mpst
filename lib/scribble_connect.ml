include Base

module MPST = struct
  type 'a mpst = MPST of 'a list lazy_t

  exception RoleNotEnabled

  type ('v1, 'v2, 's1, 's2) lens = {get : 's1 -> 'v1; put : 's1 -> 'v2 -> 's2}
  type ('a, 'b, 'c, 'd, 'r) role = ('a, 'b, 'c, 'd) lens * 'r

  type _ select = Select__
  type _ branch = Branch__
  type _ accept = Accept__
  type _ request = Request__
  type _ disconnect = Disconnec__
  type close = Close__

  type ('r, 'k) conn = {mutable conn:'k option; origin:'r option}

  type _ sess =
    | Select : 'a -> 'a select sess
    | SelectMulti : 'a -> 'a select sess
    | Branch : ('r1 * (unit -> 'a Lwt.t)) -> ('r1 * 'a) branch sess (* "fancy" type rep. (hiding Lwt.t) *)
    | DummyBranch : 'a branch sess
    | Request : 'a -> 'a request sess
    | Accept : 'r1 * ('k1 -> 'a Lwt.t) -> ('r1 * ('k1 -> 'a)) accept sess
    | Disconnect : 'a -> 'a disconnect sess
    | Close : close sess

  type ('d1, 'd, 'f1, 'f) commm = {sender:'d -> 'd1; receiver:'f -> unit -> 'f1 Lwt.t}

  type (_,_,_,_,_,_) commm2 = Commm2 : {sender2 : 'x1 * 'x2 -> 'l; receiver2: ('y1 * 'y2) -> unit -> 'r Lwt.t} -> ('l, 'x1, 'x2, 'r, 'y1, 'y2) commm2

  let mklabel o1 o2 write read (k1, k2) =
    {sender=(fun x -> o1 (fun v-> (write k1 v:unit);x));
     receiver=(fun x () -> Lwt.map (fun v -> o2(v,x)) (read k2))}

  let msg_ write read (k1, k2) =
    mklabel (fun g -> object method msg=g end) (fun x -> `msg x) write read (k1, k2)

  let left_ write read (k1, k2) =
    mklabel (fun g -> object method left=g end) (fun x -> `left x) write read (k1, k2)

  let right_ write read (k1, k2) =
    mklabel (fun g -> object method right=g end) (fun x -> `right x) write read (k1, k2)

  let dummyconn = ({conn=Some (); origin=None}, {conn=Some (); origin=None})

  let msg () =
    let st, push = Lwt_stream.create () in
    msg_ (fun _ v -> push (Some v)) (fun _ -> Lwt_stream.next st) dummyconn

  let left () =
    let st, push = Lwt_stream.create () in
    left_ (fun _ v -> push (Some v)) (fun _ -> Lwt_stream.next st) dummyconn

  let right () =
    let st, push = Lwt_stream.create () in
    right_ (fun _ v -> push (Some v)) (fun _ -> Lwt_stream.next st) dummyconn

  let flatten : 'a mpst -> 'a mpst -> 'a mpst = fun (MPST m1) (MPST m2) ->
    MPST (lazy (Lazy.force m1 @ Lazy.force m2))

  let (-->) : type d1 f1 d f s t u r1 r2 c.
       (d sess, (r2 * d1) select sess, s mpst, t mpst, r1) role
    -> (f sess, (r1 * f1) branch sess, t mpst, u mpst, r2) role
    -> (d1, d sess, f1, f sess) commm
    -> s mpst
    -> u mpst =
    fun (r1_l, r1) (r2_l, r2) comm sobj ->
      let d = lazy (r1_l.get sobj) in
      let tobj = r1_l.put sobj (Select (r2, comm.sender (Lazy.force d))) in
      let f = lazy (r2_l.get tobj) in
      let uobj = r2_l.put tobj (Branch (r1, fun () -> comm.receiver (Lazy.force f) ())) in
      uobj

  let (-!->) : type d1 f1 d f s t u r1 r2 k1 k2.
       (d sess, (r2 * (k1 -> d1)) request sess, s mpst, t mpst, r1) role
    -> (f sess, (r1 * (k2 -> f1)) accept sess, t mpst, u mpst, r2) role
    -> ((r1, k1) conn * (r2, k2) conn -> (d1,  d sess, f1, f sess) commm)
    -> ((r1, k1) conn * (r2, k2) conn -> s mpst)
    -> u mpst =
    fun (r1_l, r1) (r2_l, r2) comm sobjf ->
      let k1r, k2r = {conn=None; origin=Some r1}, {conn=None; origin=Some r2} in
      let sobj = sobjf (k1r, k2r) in
      let comm = comm (k1r, k2r) in
      let d = lazy (r1_l.get sobj) in
      let tobj = r1_l.put sobj (Request (r2, fun k -> k1r.conn <- Some k; comm.sender (Lazy.force d))) in
      let f = lazy (r2_l.get tobj) in
      let uobj = r2_l.put tobj (Accept (r1, fun k -> k2r.conn <- Some k; comm.receiver (Lazy.force f) ())) in
      uobj

  let discon : type d1 f1 d f s t u r1 r2 k1 k2.
       (d sess, (r2 * (r1, k1) conn * d sess) disconnect sess, s mpst, t mpst, r1) role
    -> (f sess, (r1 * (r2, k2) conn * f sess) disconnect sess, t mpst, u mpst, r2) role
    -> ((r1, k1) conn * (r2, k2) conn)
    -> s mpst
    -> u mpst =
    fun (r1_l, r1) (r2_l, r2) (k1r, k2r) sobj ->
      let d = lazy (r1_l.get sobj) in
      let tobj = r1_l.put sobj (Disconnect (r2, k1r, (Lazy.force d))) in
      let f = lazy (r2_l.get tobj) in
      let uobj = r2_l.put tobj (Disconnect (r1, k2r, (Lazy.force f))) in
      uobj

  let mklabel2 o1 o21 o22 write read =
    Commm2
      {sender2=(fun (x,y)-> o1 (fun v -> write (Left v);x) (fun v -> write (Right y);y));
       receiver2=(fun (x,y) () -> Lwt.map (function Left v -> o21(v,x) | Right v -> o22(v,y)) (read ()))}

  let leftright push st =
    mklabel2 (fun f g -> object method left=f method right=g end) (fun x -> `left x) (fun x -> `right x) push st

  let leftright () =
    let st, push = Lwt_stream.create () in
    leftright (fun v -> push (Some v)) (fun () -> Lwt_stream.next st)

  let (-!%%->) : type r1 r2 d1 d2 f1 f2 ss s1 s2 t t1 t2 u v1 v2 r l k1 k2.
       (close sess, (r2 * l) select sess, ss mpst, t mpst, r1) role
    -> (close sess, (r1 * r) branch sess, t mpst, u mpst, r2) role
    -> (unit -> (l, d1 sess, d2 sess, r, f1 sess, f2 sess) commm2)
    -> l1:(((d1 sess, close sess, s1 mpst, t1 mpst, r1) role * (f1 sess, close sess, t1 mpst, ss mpst, r2) role) * s1 mpst)
    -> l2:(((d2 sess, close sess, s2 mpst, t2 mpst, r1) role * (f2 sess, close sess, t2 mpst, ss mpst, r2) role) * s2 mpst)
    -> u mpst =
    fun (r1_l, r1) (r2_l, r2) dlab
        ~l1:(((r1_ll,_),(r2_ll,_)), s1obj)
        ~l2:(((r1_lr,_),(r2_lr,_)), s2obj) ->
      let Commm2 dlab = dlab () in
      let d1,t1obj = lazy (r1_ll.get s1obj), r1_ll.put s1obj Close in
      let f1,ssobj_l = lazy (r2_ll.get t1obj), r2_ll.put t1obj Close in
      let d2,t2obj = lazy (r1_lr.get s2obj), r1_lr.put s2obj Close in
      let f2,ssobj_r = lazy (r2_lr.get t2obj), r2_lr.put t2obj Close in
      let dd = SelectMulti (r2, dlab.sender2 (Lazy.force d1, Lazy.force d2)) in
      let ff = Branch (r1, fun () -> dlab.receiver2 (Lazy.force f1, Lazy.force f2) ()) in
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

  let request : 'r 'l 'v 's 'k. 'r -> ((< .. > as 'l) -> 'v -> 's sess) -> 'v -> 'k -> ('r * ('k -> 'l)) request sess -> 's sess =
    fun _ g v k (Request (_,r)) ->
    g (r k) v

  let receive : 'r 'l. 'r -> ('r * 'l) branch sess -> 'l Lwt.t =
    fun _ s -> match s with
               | Branch (_,l) -> l ()
               | DummyBranch -> failwith "dummy branch encountered"

  let accept : 'r 'l 'k. 'r -> 'k -> ('r * ('k -> 'l)) accept sess -> 'l Lwt.t =
    fun _ k s -> match s with
               | Accept (_,l) -> l k

  let disconnect : 'r 'k. 'r -> ('k -> unit) -> ('r * (_, 'k) conn * 's sess) disconnect sess -> 's sess =
    fun _ f (Disconnect (_, kr, s)) -> (match kr.conn with Some k -> f k; kr.conn <- None | None -> failwith "connection already closed"); s

  let close : close sess -> unit = fun _ -> ()

  let rec unify : type s. s sess -> s sess -> s sess = fun s1 s2 ->
    begin
      match s1, s2 with
      | Branch (r1,a1), Branch (r2,a2) -> if a1==a2 then Branch (r1,a1) else Branch (r1, fun () -> Lwt.choose [a1 (); a2 ()])
      | Accept (r1,a1), Accept (r2,a2) -> if a1==a2 then Accept (r1,a1) else Accept (r1, fun k -> Lwt.choose [a1 k; a2 k])
      | b, DummyBranch -> b
      | DummyBranch, b -> b
      | Close, Close -> Close
      | Disconnect a, _ -> Disconnect a
      | (SelectMulti _) as x, SelectMulti _ -> x (* FIXME: must raise an exception in some cases -- *)
      | Request _, _ -> raise RoleNotEnabled
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
