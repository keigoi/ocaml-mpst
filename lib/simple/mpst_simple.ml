module LinMonad = LinMonad

module type LIN = sig
  type 'a lin
  val mklin : 'a -> 'a lin
end

type close = unit

type _ sess =
  | Send : (< .. > as 'obj) -> 'obj sess
  | Recv : ([>] as 'var) Event.event -> 'var Event.event sess
  | Close : close sess

let unsess : type t. t sess -> t = function
  | Send(obj) -> obj
  | Recv(f) -> f
  | Close -> ()

let close () =
  ()

type ('la,'lb,'va,'vb) label =
    {make_obj: 'va -> 'la;
     make_var: 'vb -> 'lb}

type _ eps =
  Cons : 'x sess * 'xs eps lazy_t -> ('x * 'xs) eps
| Nil : unit eps

type (_,_,_,_) lens =
  | Fst  : ('a, 'b, ('a * 'xs) eps, ('b * 'xs) eps) lens
  | Next : ('a,'b, 'xs eps,'ys eps) lens
           -> ('a,'b, ('x * 'xs) eps, ('x * 'ys) eps) lens

let slot_head : type hd tl. (hd * tl) eps lazy_t -> hd sess = fun sl ->
  match sl with
  | lazy (Cons(hd,_)) -> hd

let slot_tail : type hd tl. (hd * tl) eps lazy_t -> tl eps lazy_t = fun sl ->
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


exception RoleNotEnabled

let merge_sess : type s. s sess -> s sess -> s sess = fun l r ->
  match l, r with
  | Send _, Send _ -> raise RoleNotEnabled
  | Recv f, Recv g -> Recv (Event.choose [f; g])
  | Close, Close -> Close
  | Close, Recv _ -> assert false
  | Recv _, Close -> assert false
  | Recv _, Send _ -> assert false
  | Send _, Recv _ -> assert false


let rec merge_eps : type t. t eps lazy_t -> t eps lazy_t -> t eps lazy_t = fun ls rs ->
  lazy begin
      match ls, rs with
      | lazy (Cons(hd_l,tl_l)), lazy (Cons(hd_r, tl_r)) ->
         (Cons(merge_sess hd_l hd_r, merge_eps tl_l tl_r))
      | lazy Nil, _ ->
         Nil
    end

type ('l, 'r, 'lr) obj_merge =
    {obj_merge: 'l -> 'r -> 'lr}

let send_obj : 'obj. (< .. > as 'obj) sess -> 'obj = function[@warning "-8"]
  | Send obj -> obj

let goto l =
  lazy (Lazy.force @@ Lazy.force l)

let a = {role={make_obj=(fun v->object method role_a=v end);
               make_var=(fun v->(`role_a(v):[`role_a of _]))}; (* explicit annotataion is mandatory *)
         lens=Fst}
let b = {role={make_obj=(fun v->object method role_b=v end);
               make_var=(fun v->(`role_b(v):[`role_b of _]))}; (* explicit annotataion is mandatory *)
         lens=Next Fst}
let c = {role={make_obj=(fun v->object method role_c=v end);
               make_var=(fun v->(`role_c(v):[`role_c of _]))}; (* explicit annotataion is mandatory *)
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
  {obj_merge=(fun l r -> object method role_b=m.obj_merge l#role_b r#role_b end)}
let b_or_c =
  {obj_merge=(fun l r -> object method role_b=l#role_b method role_c=r#role_c end)}

(* let finish =
 *   let rec fini = lazy (Cons(Close, fini)) in
 *   Lazy.from_val (Lazy.force fini) *)

let get_sess r g = unsess (lens_get r.lens g)

let one xs = Lazy.from_val (Cons(Close, xs))
let nil = Lazy.from_val Nil

let finish = one @@ one @@ one @@ nil

module MakeGlobal(X:LIN) = struct

  let comm a b label g0 =
    let ch = Event.new_channel () in
    let obj =
      b.role.make_obj @@
        label.make_obj
          (fun v ->
            Event.sync (Event.send ch v);
            X.mklin @@ unsess (lens_get a.lens g0)) in
    let g1 = lens_put a.lens g0 (Send obj) in
    let ev =
      let ev = Event.receive ch in
      Event.wrap ev (fun v -> a.role.make_var @@ label.make_var (v, X.mklin @@ unsess (lens_get b.lens g1))) in
    let g2 = lens_put b.lens g1 (Recv ev) in
    g2

  let (-->) = comm

  let choice_at a merge (al,cl) (ar,cr) =
    let sal, sar = lens_get al.lens cl, lens_get ar.lens cr in
    let cl, cr = lens_put al.lens cl Close,
                 lens_put ar.lens cr Close in
    let c = merge_eps cl cr in
    let lr = Send (merge.obj_merge (send_obj sal) (send_obj sar)) in
    lens_put a.lens c lr
end

include MakeGlobal(struct type 'a lin = 'a let mklin x = x end)

module Lin : sig
  open LinMonad

  val comm :
      ('a, [>  ] as 'var, 'b, 'c, < .. > as 'd, 'e, 'f) role ->
      ('d, 'g, 'h, 'i, 'var Event.event, 'f, 'j) role ->
      ('h, 'b, 'k -> 'c lin, 'k * 'i lin) label -> 'e lazy_t -> 'j lazy_t
  val ( --> ) :
      ('a, [>  ] as 'var, 'b, 'c, < .. > as 'd, 'e, 'f) role ->
      ('d, 'g, 'h, 'i, 'var Event.event, 'f, 'j) role ->
      ('h, 'b, 'k -> 'c lin, 'k * 'i lin) label -> 'e lazy_t -> 'j lazy_t
  val choice_at :
      ('a, 'b, 'c, 'd, < .. > as 'e, 'f eps, 'g) role ->
      (< .. > as 'h, < .. > as 'i, 'e) obj_merge ->
      ('j, 'k, 'l, 'h, close, 'm, 'f eps) role * 'm lazy_t ->
      ('n, 'o, 'p, 'i, close, 'q, 'f eps) role * 'q lazy_t -> 'g lazy_t

  type 'g global

  val create_global : (unit -> 'g eps lazy_t) -> [>] list -> 'g global

  val connect :
    'g global ->
    (_, _, unit, 's, _, 'g eps, _) role ->
    ('pre, 'pre, 's lin) monad

  val send :
    ((< .. > as 'obj) -> 'v data -> 's lin) ->
    'v ->
    ('obj lin, empty, 'pre, 'post) lens ->
    ('pre, 'post, 's lin) monad

  val deleg_send :
    ((< .. > as 'obj) -> 't sess lin -> 's sess lin) ->
    ('t sess lin, empty, 'pre, 'mid) lens ->
    ('obj lin, empty, 'mid, 'post) lens ->
    ('pre, 'post, 's sess lin) monad

  val receive :
    ('var Event.event lin, empty, 'pre, 'post) lens ->
    ('pre, 'post, 'var lin) monad

  val close :
    (close lin, empty, 'pre, 'post) lens ->
    ('pre, 'post, unit data) monad

end
  = struct
  module L = struct
    type 't lin = 't LinMonad.lin
    let mklin a = {LinMonad.__lindata=a}
  end

  module G = MakeGlobal(L)
  include G


  let stream_tee stream =
    let m = Mutex.create () in
    let next self other _i =
      Mutex.lock m;
      let value =
        if Queue.is_empty self then begin
            let value = Stream.next stream in
            Queue.add value other;
            Some value
          end else begin
            Some (Queue.take self)
          end
      in
      Mutex.unlock m;
      value
    in
    let q1 = Queue.create () in
    let q2 = Queue.create () in
    (Stream.from (next q1 q2), Stream.from (next q2 q1))

  type 'g global =
    {locals:(Obj.t * 'g eps lazy_t Stream.t) list}

  let create_global f rs =
    let st0 = Stream.from (fun _ -> Some (f ())) in
    match rs with
    | [] -> failwith "empty roles"
    | r::rs ->
       let st0, locals =
         List.fold_left
           (fun (st,xs) r ->
             let s1, s2 = stream_tee st in
             s2, (Obj.repr r, s1)::xs)
           (st0,[])
           rs
       in
       {locals=(Obj.repr r,st0)::locals}

  let connect {locals} {role;lens} =
    {LinMonad.__run=
       fun pre->
       let st = List.assoc (Obj.repr (role.make_var ())) locals in
       Stream.next st |> (fun g ->
       (pre, {LinMonad.__lindata=unsess @@ lens_get lens g}))
    }

  let send sel v lens =
    let open LinMonad in
    {__run=
       fun pre ->
       let {__lindata=obj} = lens_get lens pre in
       let s = (sel obj {data=v}).__lindata in
       (lens_put lens pre Empty, {__lindata=s})
    }

  let deleg_send sel lens1 lens0 =
    let open LinMonad in
    {__run=
       fun pre ->
       let t = lens_get lens1 pre in
       let mid = lens_put lens1 pre Empty in
       let {__lindata=obj} = lens_get lens0 mid in
       let s = (sel obj t).__lindata in
       (lens_put lens0 mid Empty, {__lindata=s})
    }

  let receive lens =
    let open LinMonad in
    {__run=
       fun pre ->
       let {__lindata=s} = lens_get lens pre in
       let ls = Event.sync s in
       (lens_put lens pre Empty, {__lindata=ls})
    }

  let close lens =
    let open LinMonad in
    {__run=
       fun pre ->
       let {__lindata=s} = lens_get lens pre in
       let () = close s in
       (lens_put lens pre Empty, {data=()})
    }
end
