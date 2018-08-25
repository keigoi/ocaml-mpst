open Base
open Session

type 'a mpst = MPST of 'a list lazy_t

exception RoleNotEnabled

type ('a, 'b, 'c, 'd, 'r) role = ('a, 'b, 'c, 'd) lens * 'r
type ('d1, 'd, 'f1, 'f) commm = {sender:'d -> 'd1; receiver:'f -> unit -> 'f1 Lwt.t}
type ('l, 'x1, 'x2, 'r, 'y1, 'y2) commm2 = {sender2 : 'x1 * 'x2 -> 'l; receiver2: ('y1 * 'y2) -> unit -> 'r Lwt.t}



let (-->) : type d1 f1 d f s t u r1 r2 c.
                 (d sess, (r2 * d1) select sess, s mpst, t mpst, r1) role
                 -> (f sess, (r1 * f1) branch sess, t mpst, u mpst, r2) role
                 -> (d1, d sess, f1, f sess) commm
                 -> s mpst
                 -> u mpst =
  fun (r1_l, r1) (r2_l, r2) comm sobj ->
  let d = lazy (r1_l.get sobj) in
  let tobj = r1_l.put sobj (Select (lazy (r2, comm.sender (Lazy.force d)))) in
  let f = lazy (r2_l.get tobj) in
  let uobj = r2_l.put tobj (Branch (r1, [(fun () -> comm.receiver (Lazy.force f) ())])) in
  uobj

let ( *--> ) : type d1 f1 d f s t u r1 r2 r3 c.
                 ((r3 * d) branch sess, ((r2 * r3) * d1) selectbranch sess, s mpst, t mpst, r1) role
                 -> (f sess, (r1 * f1) branch sess, t mpst, u mpst, r2) role
                 -> (d1, d proc, f1, f sess) commm
                 -> s mpst
                 -> u mpst =
  fun (r1_l, r1) (r2_l, r2) comm sobj ->
  let d = lazy (r1_l.get sobj) in
  let tobj = r1_l.put sobj (SelectBranch (lazy ((r2, (Obj.magic ():r3)), (comm.sender @@ mkproc (fun () -> _receive (Lazy.force d)))))) in
  let f = lazy (r2_l.get tobj) in
  let uobj = r2_l.put tobj (Branch (r1, [(fun () -> comm.receiver (Lazy.force f) ())])) in
  uobj

let flatten : 'a mpst -> 'a mpst -> 'a mpst = fun (MPST m1) (MPST m2) ->
  MPST (lazy (Lazy.force m1 @ Lazy.force m2))

let (-%%->) : type r1 r2 d1 d2 f1 f2 ss s1 s2 t t1 t2 u v1 v2 r l k1 k2.
                     (close sess, (r2 * l) select sess, ss mpst, t mpst, r1) role
                   -> (close sess, (r1 * r) branch sess, t mpst, u mpst, r2) role
                   -> (l, d1 sess, d2 sess, r, f1 sess, f2 sess) commm2
                   -> l1:(((d1 sess, close sess, s1 mpst, t1 mpst, r1) role * (f1 sess, close sess, t1 mpst, ss mpst, r2) role) * s1 mpst)
                   -> l2:(((d2 sess, close sess, s2 mpst, t2 mpst, r1) role * (f2 sess, close sess, t2 mpst, ss mpst, r2) role) * s2 mpst)
                   -> u mpst =
  fun (r1_l, r1) (r2_l, r2) comm
      ~l1:(((r1_ll,_),(r2_ll,_)), s1obj)
      ~l2:(((r1_lr,_),(r2_lr,_)), s2obj) ->
  let d1,t1obj = lazy (r1_ll.get s1obj), r1_ll.put s1obj Close in
  let f1,ssobj_l = lazy (r2_ll.get t1obj), r2_ll.put t1obj Close in
  let d2,t2obj = lazy (r1_lr.get s2obj), r1_lr.put s2obj Close in
  let f2,ssobj_r = lazy (r2_lr.get t2obj), r2_lr.put t2obj Close in
  let dd = Select (lazy (r2, comm.sender2 (Lazy.force d1, Lazy.force d2))) in
  let ff = Branch (r1, [(fun () -> comm.receiver2 (Lazy.force f1, Lazy.force f2) ())]) in
  let tobj_l = r1_l.put ssobj_l dd in
  let uobj_l = r2_l.put tobj_l ff in
  let tobj_r = r1_l.put ssobj_r dd in
  let uobj_r = r2_l.put tobj_r ff in
  flatten uobj_l uobj_r

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
  let uobj = r2_l.put tobj (Accept (r1, [(fun k -> k2r.conn <- Some k; comm.receiver (Lazy.force f) ())])) in
  uobj


let (-?->) : type d1 f1 d f s t u r1 r2 c k1 k2.
                 (d sess, (r2 * d1) select sess, s mpst, t mpst, r1) role
                 -> (f sess, (r1 * f1) branch sess, t mpst, u mpst, r2) role
                 -> (d1, (r2 * (r1,k1) conn * d sess) disconnect sess, f1, (r1 * (r2,k2) conn * f sess) disconnect sess) commm
                 -> (r2,k2) conn * (r1,k1) conn
                 -> s mpst
                 -> u mpst =
  fun (r1_l, r1) (r2_l, r2) comm (k2r, k1r) sobj ->
  let d = lazy (r1_l.get sobj) in
  let tobj = r1_l.put sobj (Select (lazy (r2, comm.sender (Disconnect (r2, k1r, (Lazy.force d)))))) in
  let f = lazy (r2_l.get tobj) in
  let uobj = r2_l.put tobj (Branch (r1, [(fun () -> comm.receiver (Disconnect (r1, k2r, (Lazy.force f))) ())])) in
  uobj

let (-!%%->) : type r1 r2 d1 d2 f1 f2 ss s1 s2 t t1 t2 u v1 v2 r l k1 k2.
                     (close sess, (r2 * (k1 -> l)) request sess, ss mpst, t mpst, r1) role
                   -> (close sess, (r1 * (k2 -> r)) accept sess, t mpst, u mpst, r2) role
                   -> ((r1, k1) conn * (r2, k2) conn -> (l, d1 sess, d2 sess, r, f1 sess, f2 sess) commm2)
                   -> l1:(((d1 sess, close sess, s1 mpst, t1 mpst, r1) role * (f1 sess, close sess, t1 mpst, ss mpst, r2) role) * ((r1, k1) conn * (r2, k2) conn -> s1 mpst))
                   -> l2:(((d2 sess, close sess, s2 mpst, t2 mpst, r1) role * (f2 sess, close sess, t2 mpst, ss mpst, r2) role) * ((r1, k1) conn * (r2, k2) conn -> s2 mpst))
                   -> u mpst =
  fun (r1_l, r1) (r2_l, r2) comm
      ~l1:(((r1_ll,_),(r2_ll,_)), s1obj)
      ~l2:(((r1_lr,_),(r2_lr,_)), s2obj) ->
  let k1r, k2r = {conn=None; origin=Some r1}, {conn=None; origin=Some r2} in
  let s1obj, s2obj = s1obj (k1r, k2r), s2obj (k1r, k2r) in
  let comm = comm (k1r, k2r) in
  let d1,t1obj = lazy (r1_ll.get s1obj), r1_ll.put s1obj Close in
  let f1,ssobj_l = lazy (r2_ll.get t1obj), r2_ll.put t1obj Close in
  let d2,t2obj = lazy (r1_lr.get s2obj), r1_lr.put s2obj Close in
  let f2,ssobj_r = lazy (r2_lr.get t2obj), r2_lr.put t2obj Close in
  let dd = Request (r2, fun k -> k1r.conn <- Some k; comm.sender2 (Lazy.force d1, Lazy.force d2)) in
  let ff = Accept (r1, [(fun k -> k2r.conn <- Some k; comm.receiver2 (Lazy.force f1, Lazy.force f2) ())]) in
  let tobj_l = r1_l.put ssobj_l dd in
  let uobj_l = r2_l.put tobj_l ff in
  let tobj_r = r1_l.put ssobj_r dd in
  let uobj_r = r2_l.put tobj_r ff in
  flatten uobj_l uobj_r

let (-?%%->) : type r1 r2 d1 d2 f1 f2 ss s1 s2 t t1 t2 u v1 v2 r l k1 k2.
                     (close sess, (r2 * l) select sess, ss mpst, t mpst, r1) role
                   -> (close sess, (r1 * r) branch sess, t mpst, u mpst, r2) role
                   -> (l, (r2 * (r1,k1) conn * d1 sess) disconnect sess, (r2 * (r1,k1) conn * d2 sess) disconnect sess,
                       r, (r1 * (r2,k2) conn * f1 sess) disconnect sess, (r1 * (r2,k2) conn * f2 sess) disconnect sess) commm2
                   -> (r2,k2) conn * (r1,k1) conn
                   -> l1:(((d1 sess, close sess, s1 mpst, t1 mpst, r1) role * (f1 sess, close sess, t1 mpst, ss mpst, r2) role) * s1 mpst)
                   -> l2:(((d2 sess, close sess, s2 mpst, t2 mpst, r1) role * (f2 sess, close sess, t2 mpst, ss mpst, r2) role) * s2 mpst)
                   -> u mpst =
  fun (r1_l, r1) (r2_l, r2) comm (k2r, k1r)
      ~l1:(((r1_ll,_),(r2_ll,_)), s1obj)
      ~l2:(((r1_lr,_),(r2_lr,_)), s2obj) ->
  let d1,t1obj = lazy (r1_ll.get s1obj), r1_ll.put s1obj Close in
  let f1,ssobj_l = lazy (r2_ll.get t1obj), r2_ll.put t1obj Close in
  let d2,t2obj = lazy (r1_lr.get s2obj), r1_lr.put s2obj Close in
  let f2,ssobj_r = lazy (r2_lr.get t2obj), r2_lr.put t2obj Close in
  let dd = Select (lazy (r2, comm.sender2 (Disconnect (r2, k1r, Lazy.force d1), (Disconnect (r2, k1r, Lazy.force d2))))) in
  let ff = Branch (r1, [(fun () -> comm.receiver2 (Disconnect (r1, k2r, Lazy.force f1), (Disconnect (r1, k2r, Lazy.force f2))) ())]) in
  let tobj_l = r1_l.put ssobj_l dd in
  let uobj_l = r2_l.put tobj_l ff in
  let tobj_r = r1_l.put ssobj_r dd in
  let uobj_r = r2_l.put tobj_r ff in
  flatten uobj_l uobj_r

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

let dummy_close : 'a 'r. (close sess, close sess, 'a mpst, 'a mpst, 'r) role -> 'a mpst -> 'a mpst =
  fun (l,_) aobj ->
  l.put aobj Close

let dummy_receive : 'a 'l 'r. ('l branch sess, 'l branch sess, 'a mpst, 'a mpst, 'r1) role -> 'a mpst -> 'a mpst =
  fun (l,_) aobj ->
  l.put aobj DummyBranch (* never fires *)

let unmpst (MPST (lazy m)) = m

let loop_ (m : 'a mpst lazy_t) =
  MPST (lazy (unmpst (Lazy.force m)))

let get_sess : 's 'a. ('s sess, _, 'a mpst, _, _) role -> 'a mpst -> 's sess = fun (l,_) s -> l.get s


let remove_dups ls1 ls2 =
  List.fold_left (fun ls l -> if List.exists (fun x -> x==l) ls then ls else l::ls) ls1 ls2

let rec unify : type s. s sess -> s sess -> s sess = fun s1 s2 ->
  begin
    match s1, s2 with
    | Branch (r1,ls1), Branch (_,ls2) ->
       Branch (r1, remove_dups ls1 ls2) (* remove duplicates *)
    | Accept (r1,ls1), Accept (_,ls2) ->
       Accept (r1, remove_dups ls1 ls2) (* remove duplicates *)
    | b, DummyBranch -> b
    | DummyBranch, b -> b
    | Close, Close -> Close
    | Select x, Select y -> if x==y then Select x else raise RoleNotEnabled
    | SelectBranch x, SelectBranch y -> if x==y then SelectBranch x else raise RoleNotEnabled
    | Request x, Request y ->  if x==y then Request x else raise RoleNotEnabled
    | Disconnect _, _ -> raise RoleNotEnabled
  end

let rec uget : 's 'a. ('s sess, _, 'a, _) lens -> 'a mpst -> 's sess = fun l (MPST (lazy xs)) ->
  List.fold_left unify (l.get (List.hd xs)) (List.map l.get (List.tl xs))

module Labels = struct
  let mklabel o1 o2 write read (k1, k2) =
    {sender=(fun x -> o1 (fun v-> (write k1 v:unit);x));
     receiver=(fun x () -> Lwt.map (fun v -> o2(v,x)) (read k2))}

  let mklabel2 o1 o21 o22 write read (k1, k2) =
    {sender2=(fun (x,y)-> o1 (fun v -> write k1 (Left v);x) (fun v -> write k1 (Right v);y));
     receiver2=(fun (x,y) () -> Lwt.map (function Left v -> o21(v,x) | Right v -> o22(v,y)) (read k2))}
end
