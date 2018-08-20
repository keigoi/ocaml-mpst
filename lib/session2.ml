include Base

module MPST = struct
  type 'a mpst = MPST : 'a lazy_t -> 'a mpst | Nondet : ('a mpst * 'a mpst) lazy_t -> 'a mpst
  type ('v1, 'v2, 's1, 's2) lens = {get : 's1 -> 'v1; put : 's1 -> 'v2 -> 's2}
  type ('l, 'x) label = Label of ('x -> 'l)
  type ('a, 'b, 'c, 'd, 'r) role = ('a, 'b, 'c, 'd) lens * 'r
  type ('d1, 'd, 'f1, 'f) dlabel = {slabel:'d -> 'd1; rlabel:'f -> 'f1}

  type _ select = Select
  type _ branch = Branch
  type close = Close

  let msg : (<msg : 'g>, 'g, [`msg of 'h], 'h) dlabel =
    {slabel=(fun x -> object method msg=x end); rlabel=(fun x -> `msg(x))}

  let left : (<left: 'g>, 'g, [>`left of 'h | `right of _], 'h) dlabel =
    {slabel=(fun x -> object method left=x end); rlabel=(fun x -> `left(x))}

  let right : (<right: 'g>, 'g, [>`left of _ | `right of 'h], 'h) dlabel =
    {slabel=(fun x -> object method right=x end); rlabel=(fun x -> `right(x))}

  type _ sess =
    | Select : 'a -> 'a select sess
    | Branch : 'a Lwt.t -> 'a branch sess
    | Close : close sess

  let rec unify : type s. s sess -> s sess -> s sess = fun s1 s2 ->
    match s1, s2 with
    | Branch a1, Branch a2 -> Branch (Lwt.choose [a1; a2])
    | Close, Close -> Close
    | Select _, Select _ -> failwith "bad global type"

  let rec get_sess : ('s, _, 'a, _) lens -> 'a mpst -> 's = fun l m ->
    match m with
    | MPST (lazy m) -> l.get m
    | Nondet (lazy (m1, m2)) -> unify (get_sess l m1) (get_sess l m2)

  let rec put_sess : (_, 't, 'a, 'b) lens -> 'a mpst -> 't -> 'b mpst = fun l m b ->
    match m with
    | MPST m -> MPST (lazy (l.put (Lazy.force m) b))
    | Nondet p -> Nondet (lazy (let lazy (m1,m2) = p in (put_sess l m1 b, put_sess l m2 b)))

  let (-->) : type d1 f1 d f s t u v r1 r2.
       (d sess, (r2 * d1) select sess, s, t, r1) role
    -> (f sess, (r1 * f1) branch sess, t, u, r2) role
    -> (d1, v -> d sess, f1, v * f sess) dlabel
    -> s mpst
    -> u mpst =
    fun (r1_l, r1) (r2_l, r2) dlab sobj ->
      let t,u = Lwt.wait () in
      let d = get_sess r1_l sobj in
      let tobj = put_sess r1_l sobj (Select (r2, dlab.slabel (fun v -> Lwt.wakeup_later u v; d))) in
      let f = get_sess r2_l tobj in
      let uobj = put_sess r2_l tobj (Branch (Lwt.map (fun v -> (r1, dlab.rlabel (v, f))) t)) in
      uobj


  let (-%%->) : type r1 r2 d1 d2 f1 f2 ss s1 s2 t t1 t2 u v1 v2.
       (unit, (r2 * <left: v1 -> d1 sess; right: v2 -> d2 sess>) select sess, ss, t, r1) role
    -> (unit, (r1 * [`left of v1 * f1 sess | `right of v2 * f2 sess]) branch sess, t, u, r2) role
    -> left:(((d1 sess, unit, s1, t1, r1) role * (f1 sess, unit, t1, ss, r2) role) * s1 mpst)
    -> right:(((d2 sess, unit, s2, t2, r1) role * (f2 sess, unit, t2, ss, r2) role) * s2 mpst)
    -> u mpst =
    fun (r1_l, r1) (r2_l, r2)
        ~left:(((r1_ll,_),(r2_ll,_)), s1obj)
        ~right:(((r1_lr,_),(r2_lr,_)), s2obj) ->
    let f () =
      let d1,t1obj = get_sess r1_ll s1obj, put_sess r1_ll s1obj () in
      let f1,ssobj_l = get_sess r2_ll t1obj, put_sess r2_ll t1obj () in
      let d2,t2obj = get_sess r1_lr s2obj, put_sess r1_lr s2obj () in
      let f2,ssobj_r = get_sess r2_lr t2obj, put_sess r2_lr t2obj () in
      let t,u = Lwt.task () in
      let dd = Select
                 (r2,
                  object
                    method left=(fun v ->
                      let w = Lwt.wait () in
                      Lwt.wakeup_later u (Left  (v, w)); d1)
                    method right=(fun v ->
                      let w = Lwt.wait () in
                      Lwt.wakeup_later u (Right  (v, w)); d2)
                  end)
      in
      let ff =  Branch
                  (Lwt.map (function
                       | Left(v, w) -> r1,`left(v, f1)
                       | Right(v,w) -> r1,`right(v, f2)) t)
      in
      let tobj_l = put_sess r1_l ssobj_l dd in
      let uobj_l = put_sess r2_l tobj_l ff in
      let tobj_r = put_sess r1_l ssobj_r dd in
      let uobj_r = put_sess r2_l tobj_r ff in
      uobj_l, uobj_r
    in
    Nondet (lazy (f ()))

  let finish =
    MPST (Lazy.from_val (object method a=Close method b=Close method c=Close end))

  type a = A
  type b = B
  type c = C

  let a : 'a1 'a2 'b 'c. ('a1, 'a2, <a:'a1; b:'b; c:'c>, <a:'a2; b:'b; c:'c>, a) role =
    {get=(fun s -> s#a); put=(fun s v -> object method a=v method b=s#b method c=s#c end)}, A
  let b : 'a 'b1 'b2 'c. ('b1, 'b2, <a:'a; b:'b1; c:'c>, <a:'a; b:'b2; c:'c>, b) role =
    {get=(fun s -> s#b); put=(fun s v -> object method a=s#a method b=v method c=s#c end)}, B
  let c : 'a 'b 'c1 'c2. ('c1, 'c2, <a:'a; b:'b; c:'c1>, <a:'a; b:'b; c:'c2>, c) role =
    {get=(fun s -> s#c); put=(fun s v -> object method a=s#a method b=s#b method c=v end)}, C


  let send : 'r 'l 'v 's. ('r -> (< .. > as 'l)) -> ('l -> 'v -> 's sess) -> 'v -> 'r select sess -> 's sess =
    fun f g v (Select r) ->
    g (f r) v

  let receive : 'r 'l. ('r -> ([>] as 'l)) -> 'r branch sess -> 'l Lwt.t =
    fun f (Branch l) ->
    Lwt.map f l

  let close : close sess -> unit = fun _ -> ()
end
