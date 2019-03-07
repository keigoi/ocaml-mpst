open Base
open Session

type ('la,'lb,'ca,'cb,'v1, 'v2) label =
    {select_label: ('v1 -> 'ca) -> 'la;
     offer_label: 'v2 * 'cb -> 'lb}

let (-->) : type ra rb sa sb la lb c0 c1 c2 v.
      (ra, sa prot, (rb, la) send prot, c0, c1) role ->
      (rb, sb prot, (ra, lb) receive prot, c1, c2) role ->
      (la, lb, sa prot, sb prot, v, v) label ->
      c0 lazy_t -> c2 lazy_t =
  fun a b ({select_label;offer_label}) c0 ->
  let st, push = Lwt_stream.create () in
  let sa = lens_get a.lens c0 in
  let sa =
    Lazy.from_val @@
      Send (b.role, 
            select_label (fun v -> push (Some v); Lazy.force sa))
  in
  let c1 = lens_put a.lens c0 sa in
  let sb = lens_get b.lens c1 in
  let sb =
    Lazy.from_val @@
      Receive (a.role, [
            Lwt.map (fun v -> offer_label (v, Lazy.force sb)) (Lwt_stream.next st)])
  in
  let c2 = lens_put b.lens c1 sb in
  c2

(* broadcast *)
let (-->>) : type ra rb sa sb la lb c0 c1 c2 v.
      (ra, sa prot, (rb, la) send prot, c1, c2) role ->
      (rb, sb prot list, (ra, lb) receive prot list, c0, c1) role ->
      (la, lb, sa prot, sb prot, v list, v) label ->
      c0 lazy_t -> c2 lazy_t =
  fun a b ({select_label;offer_label}) c0 ->
  let sbs = lens_get b.lens c0 in
  let sbs =
    lazy
      (List.map
         (fun sb ->
           let st, push = Lwt_stream.create () in
           Receive (a.role, [
                 Lwt_stream.next st |>
                 Lwt.map (fun v -> offer_label (v, sb))
             ]),
           push
         )
         (Lazy.force sbs))
  in
  let c1 = lens_put b.lens c0 (lazy (List.map fst (Lazy.force sbs))) in
  let sa = lens_get a.lens c1 in
  let sa =
    Lazy.from_val @@
      Send (b.role, 
            select_label (fun vs ->
                List.iter2
                  (fun (_,push) v -> push (Some v))
                  (Lazy.force sbs)
                  vs;
                Lazy.force sa))
  in
  let c2 = lens_put a.lens c1 sa in
  c2

(* gather *)
let (>>--) : type ra rb sa sb la lb c0 c1 c2 v.
      (ra, sa prot list, (rb, la) send prot list, c0, c1) role ->
      (rb, sb prot, (ra, lb) receive prot, c1, c2) role ->
      (la, lb, sa prot, sb prot, v, v list) label ->
      c0 lazy_t -> c2 lazy_t =
  fun a b ({select_label;offer_label}) c0 ->
  let sas = lens_get a.lens c0 in
  let sas =
    lazy
      (List.map
         (fun sa ->
           let st, push = Lwt_stream.create () in
           Send (b.role, 
                 select_label (fun v -> push (Some v); sa)),
           st
         )
      (Lazy.force sas))
  in
  let c1 = lens_put a.lens c0 (lazy (List.map fst (Lazy.force sas))) in
  let sb = lens_get b.lens c1 in
  let sb =
    lazy (
      Receive (a.role, [
            let sts = List.map snd (Lazy.force sas) in
            let vs = Lwt_list.map_s Lwt_stream.next sts in
            Lwt.map
              (fun v -> offer_label (v, Lazy.force sb))
              vs
        ]))
  in
  let c2 = lens_put b.lens c1 sb in
  c2

let dummy_receive ra c0 =
  lens_put ra.lens c0 (Lazy.from_val DummyReceive)

let dummy_close ra c0 =
  lens_put ra.lens c0 (Lazy.from_val Close)

type ('l, 'r, 'lr) label_merge =
    {label_merge: 'l -> 'r -> 'lr}

let label : type l ks k r. (r, l) send prot lazy_t -> l =
  function
  | lazy (Send (_, l)) -> l
  | lazy Close -> assert false

let role : type l ks k r. (r, l) send prot lazy_t -> r =
  function
  | lazy (Send (r, _)) -> r
  | lazy Close -> assert false

let rec merge_ : type t. t slots -> t slots -> t slots =
  fun l r ->
  match l, r with
  | ConsProt(lazy hd_l,tl_l), ConsProt(lazy hd_r,tl_r) ->
     ConsProt (lazy (Internal.merge hd_l hd_r),
               merge_ tl_l tl_r)
  | ConsList(lazy hd_l,tl_l), ConsList(lazy hd_r,tl_r) ->
     ConsList (lazy (List.rev @@ List.rev_map2 Internal.merge hd_l hd_r),
               merge_ tl_l tl_r)
  | Nil, Nil ->
     Nil

let merge (lazy cl) (lazy cr) = lazy (merge_ cl cr)
    
let choice_at a {label_merge} (al,cl) (ar,cr) =
  let sal, sar = lens_get al.lens cl, lens_get ar.lens cr in
  let cl, cr = lens_put al.lens cl (Lazy.from_val Close), lens_put ar.lens cr (Lazy.from_val Close) in
  let c = merge cl cr in
  let lr = lazy (Send (role sar, label_merge (label sal) (label sar))) in
  lens_put a.lens c lr

let loop c0 = lazy (Lazy.force (Lazy.force c0))


