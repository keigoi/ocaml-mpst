type ('a, 'b) either = Left of 'a | Right of 'b

(* module Bin = struct
 *   type 'p wrap =
 *     Msg : ('v * 'p wrap Lwt.t) -> [`msg of 'r * 'v * 'p] wrap
 *   | BranchLeft : 'p wrap Lwt.t  -> [`branch of 'r * [> `left of 'p]] wrap
 *   | Chan : (('pp, 'rr) sess * 'p wrap Lwt.t) -> [`deleg of 'r * ('pp, 'rr) sess * 'p] wrap
 *   and ('p, 'q) sess = 'p wrap Lwt.t * 'q
 * end *)

module Local = struct
  type _ sess =
    | Send : ('r * ('v -> 's sess Lwt.t)) -> [`send of 'r * [`msg of 'v * 's]] sess
    | Recv : ('r * (unit -> ('v * 's sess) Lwt.t)) -> [`recv of 'r * [`msg of 'v * 's]] sess
    | SelectLeft : 'r * (unit -> 's sess) -> [`send of 'r * [`left of 's]] sess
    | SelectRight : 'r * (unit -> 's sess) -> [`send of 'r * [`right of 's]] sess
    | SelectLeftRight : 'r * (unit -> ('s1 sess, 's2 sess) either Lwt.t) -> [`send of 'r * [`left of 's1 | `right of 's2]] sess
    | BranchLeft : 'r * 's1 sess -> [`recv of 'r * [`left of 's1 | `right of 's2]] sess
    | BranchRight : 'r * 's2 sess -> [`recv of 'r * [`left of 's1 | `right of 's2]] sess
    | BranchLeftRight : 'r * ('s1 sess, 's2 sess) either Lwt.t -> [`recv of 'r * [`left of 's1 | `right of 's2]] sess
    | Close : [`close] sess

  type a = A
  type b = B
  type c = C
  type req = Req
  type resp = Resp

  let send : type r v s. r -> v -> [`send of r * [`msg of v * s]] sess -> s sess Lwt.t = fun _ v (Send(_,f)) ->
    f v

  let receive : type r v s. r -> [`recv of r * [`msg of v * s]] sess -> (v * s sess) Lwt.t = fun _ (Recv(_,f)) ->
    f ()

  let select_left : type r s. r -> [`send of r * [`left of s]] sess -> s sess = fun _ (SelectLeft(_,s)) ->
    s ()

  let select_right : type r s. r -> [`send of r * [`right of s]] sess -> s sess = fun _ (SelectRight(_,s)) ->
    s ()

  let branch : type r s1 s2. r -> [`recv of r * [`left of s1 |`right of s2]] sess -> (s1 sess, s2 sess) either Lwt.t =
    fun _ s ->
    match s with
    | BranchLeft(_,s) -> Lwt.return (Left s)
    | BranchRight(_,s) -> Lwt.return (Right s)
    | BranchLeftRight(_,s) -> s

  let close : [`close] sess -> unit = fun Close -> ()


  let p0 () = SelectLeft (C, fun () -> Close)


  let p1 () =
    Send (A, fun v1 -> Lwt.return @@
    Send (A, fun v2 -> Lwt.return @@
    begin
      if v2<>0 then
        BranchLeft (A, Recv (A, fun () -> Lwt.return (v1 / v2, Close)))
      else
        BranchRight (A, Close)
    end))

  let a_st () =
    Send ((), fun v1 -> Lwt.return @@
    Send ((), fun v2 -> Lwt.return @@
    begin
      if v2<>0 then
        BranchLeft ((), Recv ((), fun () -> Lwt.return (v1 / v2, Close)))
      else
        BranchRight ((), Close)
    end))

  let p2 s =
    let open Lwt in
    Send (A, fun v1 ->
    send () v1 s >>= fun s -> Lwt.return @@
    Send (A, fun v2 ->
    send () v2 s >>= fun s ->
    begin
      branch () s >>= function
      | Left s ->
         Lwt.return @@
           BranchLeft (A,
                       Recv (A, fun () -> receive () s))
      | Right _ ->
         Lwt.return @@ BranchRight (A, Close)
    end))

  let f () =
    let open Lwt in
    let s = p2 (a_st ()) in
    send A 10 s >>= fun s ->
    send A 0 s >>= fun s ->
    branch A s >>= function
    | Left s1 -> receive A s1 >>= (fun (x, s) -> close s; return (Some x))
    | Right s2 -> close s2; return None

end

module MPST = struct
  type 'a mpst = MPST of 'a
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

  let (-->) : type d1 f1 d f.
       (d sess, [`send of 'r2 * d1] sess, 't, 's, 'r1) role
    -> (f sess, [`recv of 'r1 * f1] sess, 'u, 't, 'r2) role
    -> (d1, d, f1, f) dlabel
    -> 's mpst
    -> 'u mpst =
    fun (r1_l, r1) (r2_l, r2) dlab (MPST sobj) ->
          match dlab with
          | DMsg(typ) ->
             let t, u = Lwt.wait () in
             let tobj = r1_l.put sobj (Send (r1, fun v -> Lwt.wakeup_later u v;
                                                          Lwt.return @@ r1_l.get sobj)) in
             let uobj = r2_l.put tobj (Recv (r2, fun () ->
                                                 Lwt.bind t (fun x ->
                                                     Lwt.return (x, r2_l.get tobj)))) in
             MPST uobj
          | DLeft ->
             let tobj = r1_l.put sobj (SelectLeft (r1, fun () -> r1_l.get sobj)) in
             let uobj = r2_l.put tobj (BranchLeft (r2, r2_l.get tobj)) in
             MPST uobj
          | DRight ->
             let tobj = r1_l.put sobj (SelectRight (r1, fun () -> r1_l.get sobj)) in
             let uobj = r2_l.put tobj (BranchRight (r2, r2_l.get tobj)) in
             MPST uobj

  let (-%%->) :
       (unit, [`send of 'r2 * [`left of 'd1 | `right of 'd2]] sess, 't, 'ss, 'r1) role
    -> (unit, [`recv of 'r1 * [`left of 'd1 | `right of 'f2]] sess, 'u, 't, 'r2) role
    -> left:((('d1 sess, unit, 't1, 's1, 'r1) role * (unit, 'f1, 'ss, 't1, 'r2) role) * 's1 mpst)
    -> right:((('d2 sess, unit, 't2, 's2, 'r1) role * (unit, 'f2, 'ss, 't2, 'r2) role) * 's2 mpst)
    -> 'u mpst =
    fun (r1_l, r1) (r2_l, r2)
        ~left:(((r1_ll,_),(r2_ll,_)),MPST s1obj)
        ~right:(((r1_lr,_),(r2_lr,_)),MPST s2obj) ->
    let d1,t1obj = r1_ll.get s1obj, r1_ll.put s1obj () in
    let f1,ssobj_l = r2_ll.get t1obj, r2_ll.put t1obj () in
    let d2,t2obj = r1_lr.get s1obj, r1_lr.put s2obj () in
    let f2,ssobj_r = r2_lr.get t1obj, r2_lr.put t2obj () in
    let t, u = Lwt.task () in
    let tobj = r1_l.put ssobj_l (* WTF?? *) (SelectLeftRight (r1, fun () -> Lwt.bind t @@ fun b -> Lwt.return (if b then Left d1 else Right d2))) in
    let uobj = r2_l.put tobj (failwith "TODO") in
    MPST uobj



end
