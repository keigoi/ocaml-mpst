include Base

module Local = struct
  type 'v cell = ('v Lwt.t * 'v Lwt.u) ref

  type _ sess =
    | Send : ('r * 'v cell * 's sess) -> [`send of 'r * [`msg of 'v * 's]] sess
    | Recv : ('r * 'v cell * 's sess) -> [`recv of 'r * [`msg of 'v * 's]] sess
    | SelectLeft : 'r * bool cell * 's sess -> [`send of 'r * [`left of 's]] sess
    | SelectRight : 'r * bool cell * 's sess -> [`send of 'r * [`right of 's]] sess
    | BranchLeft : 'r * bool cell * 's1 sess -> [`recv of 'r * [`left of 's1 | `right of 's2]] sess
    | BranchRight : 'r * bool cell * 's2 sess -> [`recv of 'r * [`left of 's1 | `right of 's2]] sess
    | SelectLeftRight : 'r * bool cell * 's1 sess * 's2 sess -> [`send of 'r * [`left of 's1 | `right of 's2]] sess
    | BranchLeftRight : 'r * bool cell * 's1 sess * 's2 sess -> [`recv of 'r * [`left of 's1 | `right of 's2]] sess
    | Close : [`close] sess

  let send : type r v s. r -> v -> [`send of r * [`msg of v * s]] sess -> s sess = fun _ v (Send(_,u,s)) ->
    Lwt.wakeup_later (snd !u) v; s

  let receive : type r v s. r -> [`recv of r * [`msg of v * s]] sess -> (v * s sess) Lwt.t = fun _ (Recv(_,t,s)) ->
    Lwt.bind (fst !t) @@ (fun x -> Lwt.return @@ (x, s))

  let select_left : type r s. r -> [`send of r * [`left of s]] sess -> s sess = fun _ s ->
    match s with
    | (SelectLeft(_,u,s)) -> Lwt.wakeup_later (snd !u) true; s

  let select_left_ : type r s. r -> [`send of r * [`left of s | `right of _]] sess -> s sess = fun _ s ->
    match s with
    | (SelectLeftRight(_,u,s1,_)) -> Lwt.wakeup_later (snd !u) true; s1

  let select_right : type r s. r -> [`send of r * [`right of s]] sess -> s sess = fun _ (SelectRight(_,u,s)) ->
    Lwt.wakeup_later (snd !u) false; s

  let select_right_ : type r s. r -> [`send of r * [`left of _ | `right of s]] sess -> s sess = fun _ s ->
    match s with
    | (SelectLeftRight(_,u,_,s2)) -> Lwt.wakeup_later (snd !u) false; s2

  let branch : type r s1 s2. r -> [`recv of r * [`left of s1 |`right of s2]] sess -> (s1 sess, s2 sess) either Lwt.t =
    fun _ s ->
    match s with
    | BranchLeft(_,t,s) -> Lwt.bind (fst !t) @@ fun b -> if b then Lwt.return @@ Left s else failwith "MPST: impossible: false at BranchLeft"
    | BranchRight(_,t,s) -> Lwt.bind (fst !t) @@ fun b -> if not b then Lwt.return @@ Right s else failwith "MPST: impossible: true at BranchRight"
    | BranchLeftRight(_,t,s1,s2) ->
       Lwt.bind (fst !t) @@ function
                    | true -> Lwt.return (Left s1)
                    | false -> Lwt.return (Right s2)

  let close : [`close] sess -> unit = fun Close -> ()

end

module MPST = struct
  type 'a mpst = MPST of 'a | Nondet of 'a mpst * 'a mpst
  type 'a sess = 'a Local.sess
  type ('v1, 'v2, 's1, 's2) lens = {get : 's1 -> 'v1; put : 's1 -> 'v2 -> 's2}
  type ('l, 'x) label = Label of ('x -> 'l)
  type ('a, 'b, 'c, 'd, 'r) role = ('a, 'b, 'c, 'd) lens * 'r

  type _ typ = ..

  type _ typ +=
     | Int : int typ
     | String : string typ

  type (_, _, _, _) dlabel =
    | DMsg : 't typ -> ([`msg of 't * 'g], 'g, [`msg of 't * 'h], 'h) dlabel
    | DLeft : ([`left of 'g], 'g, [`left of 'h | `right of _], 'h) dlabel
    | DRight : ([`right of 'g], 'g, [`left of _ | `right of 'h], 'h) dlabel

  let rec unify : type s. s sess -> s sess -> s sess = fun s1 s2 ->
    let open Local in
    match s1, s2 with
    | Close, Close -> Close
    | Send(r,t1,s1), Send(_,t2,s2) -> t2 := !t1; Send(r, t1, unify s1 s2)
    | Recv(r,t1,s1), Recv(_,t2,s2) -> t2 := !t1; Recv(r, t1, unify s1 s2)
    | SelectLeft(r,t1,s1), SelectLeft(_,t2,s2) -> t2 := !t1; SelectLeft(r,t1,unify s1 s2)
    | SelectRight(r,t1,s1), SelectRight(_,t2,s2) -> t2 := !t1; SelectRight(r,t2,unify s1 s2)
    | SelectLeftRight(r,t1,s11,s12), SelectLeftRight(_,t2,s21,s22) -> t2 := !t1; SelectLeftRight(r,t1,unify s11 s21,unify s12 s22)

    | BranchLeft(r,t1,s1), BranchLeft(_,t2,s2) -> t2 := !t1; BranchLeft(r,t1,unify s1 s2)
    | BranchLeft(r,t1,s1), BranchRight(_,t2,s2) -> t2 := !t1; BranchLeftRight(r,t1,s1,s2)
    | BranchLeft(r,t1,s11), BranchLeftRight(_,t2,s21,s22) -> t2 := !t1; BranchLeftRight(r,t1,unify s11 s21,s22)

    | BranchRight(r,t1,s12), BranchLeft(_,t2,s2) -> t2 := !t1; BranchLeftRight(r,t1,s2,s12)
    | BranchRight(r,t1,s12), BranchRight(_,t2,s2) -> t2 := !t1; BranchRight(r,t1,unify s12 s2)
    | BranchRight(r,t1,s12), BranchLeftRight(_,t2,s21,s22) -> t2 := !t1; BranchLeftRight(r,t1,s21,unify s12 s22)

    | BranchLeftRight(r,t1,s11,s2), BranchLeft(_,t2,s21) -> t2 := !t1; BranchLeftRight(r,t1,unify s11 s21,s2)
    | BranchLeftRight(r,t1,s1,s12), BranchRight(_,t2,s22) -> t2 := !t1; BranchLeftRight(r,t1,s1,unify s12 s22)
    | BranchLeftRight(r,t1,s11,s12), BranchLeftRight(_,t2,s21,s22) -> t2 := !t1; BranchLeftRight(r,t1,unify s11 s21,unify s12 s22)



  let rec get_sess : ('s sess, _, 'a, _, _) role -> 'a mpst -> 's sess = fun ((l,_) as r) m ->
    match m with
    | MPST m -> l.get m
    | Nondet (m1, m2) -> unify (get_sess r m1) (get_sess r m2)

  let rec put_sess : (_, 't, 'a, 'b, _) role -> 'a mpst -> 't -> 'b mpst = fun ((l,_) as r) m b ->
    match m with
    | MPST m -> MPST (l.put m b)
    | Nondet (m1, m2) -> Nondet (put_sess r m1 b, put_sess r m2 b)


  let (-->) : type d1 f1 d f s t u r1 r2.
       (d sess, [`send of r2 * d1] sess, s, t, r1) role
    -> (f sess, [`recv of r1 * f1] sess, t, u, r2) role
    -> (d1, d, f1, f) dlabel
    -> s mpst
    -> u mpst =
    fun ((_, r1) as r1_l) ((_, r2) as r2_l) dlab sobj ->
          match dlab with
          | DMsg(_) ->
             let t = ref @@ Lwt.wait () in
             let tobj = put_sess r1_l sobj (Send (r2, t, get_sess r1_l sobj)) in
             let uobj = put_sess r2_l tobj (Recv (r1, t, get_sess r2_l tobj)) in
             uobj
          | DLeft ->
             let t = ref @@ Lwt.wait () in
             let tobj = put_sess r1_l sobj (SelectLeft (r2, t, get_sess r1_l sobj)) in
             let uobj = put_sess r2_l tobj (BranchLeft (r1, t, get_sess r2_l tobj)) in
             uobj
          | DRight ->
             let t = ref @@ Lwt.wait () in
             let tobj = put_sess r1_l sobj (SelectRight (r2, t, get_sess r1_l sobj)) in
             let uobj = put_sess r2_l tobj (BranchRight (r1, t, get_sess r2_l tobj)) in
             uobj

  let (-%%->) : type r1 r2 d1 d2 f1 f2 ss s1 s2 t t1 t2 u.
       (unit, [`send of r2 * [`left of d1 | `right of d2]] sess, ss, t, r1) role
    -> (unit, [`recv of r1 * [`left of f1 | `right of f2]] sess, t, u, r2) role
    -> left:(((d1 sess, unit, s1, t1, r1) role * (f1 sess, unit, t1, ss, r2) role) * s1 mpst)
    -> right:(((d2 sess, unit, s2, t2, r1) role * (f2 sess, unit, t2, ss, r2) role) * s2 mpst)
    -> u mpst =
    fun ((_, r1) as r1_l) ((_, r2) as r2_l)
        ~left:((r1_ll,r2_ll), s1obj)
        ~right:((r1_lr,r2_lr), s2obj) ->
    let d1,t1obj = get_sess r1_ll s1obj, put_sess r1_ll s1obj () in
    let f1,ssobj_l = get_sess r2_ll t1obj, put_sess r2_ll t1obj () in
    let d2,t2obj = get_sess r1_lr s2obj, put_sess r1_lr s2obj () in
    let f2,ssobj_r = get_sess r2_lr t2obj, put_sess r2_lr t2obj () in
    let t = ref @@ Lwt.task () in
    let dd = Local.SelectLeftRight (r2, t, d1, d2) in
    let ff = Local.BranchLeftRight (r1, t, f1, f2) in
    let tobj_l = put_sess r1_l ssobj_l dd in
    let uobj_l = put_sess r2_l tobj_l ff in
    let tobj_r = put_sess r1_l ssobj_r dd in
    let uobj_r = put_sess r2_l tobj_r ff in
    Nondet (uobj_l, uobj_r)

  type a = A
  type b = B
  type c = C

  let a : 'a1 'a2 'b 'c. ('a1, 'a2, <a:'a1; b:'b; c:'c>, <a:'a2; b:'b; c:'c>, a) role =
    {get=(fun s -> s#a); put=(fun s v -> object method a=v method b=s#b method c=s#c end)}, A
  let b : 'a 'b1 'b2 'c. ('b1, 'b2, <a:'a; b:'b1; c:'c>, <a:'a; b:'b2; c:'c>, b) role =
    {get=(fun s -> s#b); put=(fun s v -> object method a=s#a method b=v method c=s#c end)}, B
  let c : 'a 'b 'c1 'c2. ('c1, 'c2, <a:'a; b:'b; c:'c1>, <a:'a; b:'b; c:'c2>, c) role =
    {get=(fun s -> s#c); put=(fun s v -> object method a=s#a method b=s#b method c=v end)}, C

  let finish : <a:[`close] sess; b:[`close] sess; c:[`close] sess> mpst =
    MPST (object method a=Local.Close method b=Local.Close method c=Local.Close end)

  let msg t = DMsg(t)
  let left = DLeft
  let right = DRight
  let int = Int
  let str = String


end
