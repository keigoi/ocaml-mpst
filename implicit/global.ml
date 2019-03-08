open Session

type ('r, 'v1, 'v2, 's1, 's2) role =
  {role:'r;
   lens:('v1, 'v2, 's1, 's2) lens;
   }

type ('la,'lb,'ca,'cb,'v1, 'v2) label =
    {select_label: ('v1 -> 'ca) -> 'la;
     offer_label: 'v2 * 'cb -> 'lb}

let (-->) : type ra rb sa sb la lb c0 c1 c2 v.
      (ra, sa sess one, (rb, la) send sess one, c0, c1) role ->
      (rb, sb sess one, (ra, lb) receive sess one, c1, c2) role ->
      (la, lb, sa sess, sb sess, v, v) label ->
      c0 lazy_t -> c2 lazy_t =
  fun a b ({select_label;offer_label}) c0 ->
  let rec sa0 = lens_get a.lens c0
  and sa =
    lazy begin
      let Sess(ksb,_) = unone @@ Lazy.force sb0 in
      let Sess(ksa,_) as sa0 = unone @@ Lazy.force sa0 in
      let k = ConnTable.get ksb a.role in
      One (Sess(ksa, Send (b.role, 
                     select_label (fun v -> rawsend k v; sa0))))
      end
  and c1 = lazy (Lazy.force @@ lens_put a.lens c0 sa)
  and sb0 = lazy (Lazy.force @@ lens_get b.lens c1) in
  let sb =
    lazy begin
      let Sess(ksa,_) = unone @@ Lazy.force sa0 in
      let Sess(ksb,_) as sb0 = unone @@ Lazy.force sb0 in
      let k = ConnTable.get ksa b.role in
      One (Sess(ksb,
                Receive (a.role, [
                 Lwt.map (fun v -> offer_label (v, sb0)) (rawreceive k)])))
      end
  in
  let c2 = lens_put b.lens c1 sb in
  c2

(* (\* broadcast *\)
 * let (-->>) : type ra rb sa sb la lb c0 c1 c2 v.
 *       (ra, sa prot one, (rb, la) send prot one, c1, c2) role ->
 *       (rb, sb prot many, (ra, lb) receive prot many, c0, c1) role ->
 *       (la, lb, sa prot, sb prot, int -> v, v) label ->
 *       c0 lazy_t -> c2 lazy_t =
 *   fun a b ({select_label;offer_label}) c0 ->
 *   let sbs = lens_get b.lens c0 in
 *   let sbs =
 *     lazy
 *       (List.map
 *          (fun sb ->
 *            Receive (a.role, [
 *                  Lwt_stream.next st |>
 *                  Lwt.map (fun v -> offer_label (v, sb))
 *              ]),
 *            push
 *          )
 *          (protmany (Lazy.force sbs)))
 *   in
 *   let c1 = lens_put b.lens c0 (lazy (ProtMany (List.map fst (Lazy.force sbs)))) in
 *   let sa = lens_get a.lens c1 in
 *   let sa =
 *     Lazy.from_val @@
 *       ProtOne (Send (b.role, 
 *             select_label (fun f ->
 *                 List.iteri
 *                   (fun i (_,push) -> push (Some (f i)))
 *                   (Lazy.force sbs);
 *                 protone (Lazy.force sa))))
 *   in
 *   let c2 = lens_put a.lens c1 sa in
 *   c2
 * 
 * (\* gather *\)
 * let (>>--) : type ra rb sa sb la lb c0 c1 c2 v.
 *       (ra, sa prot many, (rb, la) send prot many, c0, c1) role ->
 *       (rb, sb prot one, (ra, lb) receive prot one, c1, c2) role ->
 *       (la, lb, sa prot, sb prot, v, v list) label ->
 *       c0 lazy_t -> c2 lazy_t =
 *   fun a b ({select_label;offer_label}) c0 ->
 *   let sas = lens_get a.lens c0 in
 *   let sas =
 *     lazy
 *       (List.map
 *          (fun sa ->
 *            Send (b.role, 
 *                  select_label (fun v -> push (Some v); sa)),
 *            st
 *          )
 *       (protmany (Lazy.force sas)))
 *   in
 *   let c1 = lens_put a.lens c0 (lazy (ProtMany (List.map fst (Lazy.force sas)))) in
 *   let sb = lens_get b.lens c1 in
 *   let sb =
 *     lazy begin
 *       ProtOne (Receive (a.role, [
 *             let sts = List.map snd (Lazy.force sas) in
 *             let vs = Lwt_list.map_s Lwt_stream.next sts in
 *             Lwt.map
 *               (fun v -> offer_label (v, protone (Lazy.force sb)))
 *               vs
 *         ])) end
 *   in
 *   let c2 = lens_put b.lens c1 sb in
 *   c2 *)

let dummy_receive ra c0 =
  lazy begin
    let One(Sess(ks,_)) = Lazy.force @@ lens_get ra.lens c0 in
    lens_put ra.lens c0 (Lazy.from_val (One (Sess(ks,DummyReceive))))
    end

let dummy_close ra c0 =
  lazy begin
    let One(Sess(ks,_)) = Lazy.force @@ lens_get ra.lens c0 in
    lens_put ra.lens c0 (Lazy.from_val (One (Sess(ks,Close))))
    end

type ('l, 'r, 'lr) label_merge =
    {label_merge: 'l -> 'r -> 'lr}

let label : type l r. (r, l) send sess one e lazy_t -> l =
  function
  | lazy (One (Sess(_,Send (_, l)))) -> l
  | lazy (One (Sess(_,Close))) -> assert false

let role : type l r. (r, l) send sess one e lazy_t -> r =
  function
  | lazy (One (Sess(_,Send (r, _)))) -> r
  | lazy (One (Sess(_,Close))) -> assert false

let conn : type l p. p sess one e lazy_t -> ConnTable.t =
  fun (lazy (One (Sess(ks,_)))) -> ks
                                
let rec merge_ : type t. t slots lazy_t -> t slots lazy_t -> t slots lazy_t =
  fun l r ->
  match l, r with
  | lazy (Cons(lazy (One hd_l),tl_l)), lazy (Cons(lazy (One hd_r),tl_r)) ->
     lazy (Cons (lazy (One (Internal.merge hd_l hd_r)), merge_ tl_l tl_r))
  | lazy (Cons(lazy (Many hd_l),tl_l)), lazy (Cons(lazy (Many hd_r),tl_r)) ->
     lazy (Cons (lazy (Many (List.rev @@ List.rev_map2 Internal.merge hd_l hd_r)), merge_ tl_l tl_r))
  | lazy Nil, _ ->
     Lazy.from_val Nil

let choice_at a {label_merge} (al,cl) (ar,cr) =
  let sal, sar = lens_get al.lens cl, lens_get ar.lens cr in
  let cl, cr = lens_put_ al.lens cl (One (Sess(ConnTable.create (),Close))),
               lens_put_ ar.lens cr (One (Sess(ConnTable.create (),Close))) in
  let c = merge_ cl cr in
  let lr = lazy (One (Sess(conn sal, Send (role sar, label_merge (label sal) (label sar))))) in
  lens_put a.lens c lr

let loop c0 = lazy (Lazy.force (Lazy.force c0))

let get_sess r m =
  let p = lens_get_ r.lens m in
  match p with One p -> p

let get_sess_many r m =
  let p = lens_get_ r.lens m in
  match p with Many ps -> ps
