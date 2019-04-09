type ('la,'lb,'va,'vb) label =
  {name:string;
   make_obj: 'va -> 'la;
   call_obj: 'la -> 'va;
   make_var: 'vb -> 'lb}

type ('lr, 'l, 'r) obj_merge =
  {obj_merge: 'l -> 'r -> 'lr;
   obj_splitL: 'lr -> 'l;
   obj_splitR: 'lr -> 'r;
  }

type close = Close

type global =
  | Seq of string * string * string * global
  | Choice of string * global * global
  | Guard of global lazy_t
  | Goto of string
  | Finish

type 'a prot = {global:Mpst_base.Ast.global; endpoints:'a}
let print_global {global} = Mpst_base.Ast.print_global global

type 'a placeholder =
  {mutable channel: 'a Event.channel}

let create_placeholder () =
  let channel = Event.new_channel () in
  {channel}

let unify p q = q.channel <- p.channel

type _ wrap =
  | WrapSend : ((< .. > as 'obj) option -> 'obj) -> 'obj wrap
  | WrapRecv : ([>] as 'var) Event.event lazy_t -> 'var Event.event wrap
  | WrapClose : close wrap
  | WrapGuard : 'a wrap lazy_t -> 'a wrap

exception UngardedLoop

let rec find_physeq : 'a. 'a list -> 'a -> bool = fun xs y ->
  match xs with
  | x::xs -> if x==y then true else find_physeq xs y
  | [] -> false
        
let rec remove_guards : type t. t wrap lazy_t list -> t wrap lazy_t -> t wrap = fun acc w ->
  if find_physeq acc w then
    raise UngardedLoop
  else begin match Lazy.force w with
       | WrapGuard w' ->
          remove_guards (w'::acc) w'
       | w -> w
       end
  
let rec unwrap : type t. t wrap -> t = function
  | WrapSend(obj) -> obj None
  | WrapRecv(ev) -> Lazy.force ev
  | WrapClose -> Close
  | WrapGuard w ->
     let w = remove_guards [] w in
     unwrap w

let rec force_wrap : type t. t wrap -> unit = function
  | WrapSend(obj) -> ignore @@ obj None
  | WrapRecv(ev) -> ignore @@ Lazy.force ev
  | WrapClose -> ()
  | WrapGuard w -> ignore (remove_guards [] w)

let rec unwrap_send : 'a. (< .. > as 'a) wrap -> 'a option -> 'a = function
  | WrapSend(obj) -> obj
  | WrapGuard(w) ->
     let w = remove_guards [] w in
     unwrap_send w
  | _ -> assert false

type 'a e =
  | EOne : 'a wrap -> 'a wrap e
  | EList : 'a wrap list -> 'a list e

let unone (EOne v) = v

type _ seq =
  Cons : 'hd e * 'tl seq -> ('hd * 'tl) seq
| Nil : unit seq

let seq_head : type hd tl. (hd * tl) seq -> hd e =
  fun (Cons(hd,_)) -> hd

let seq_tail : type hd tl. (hd * tl) seq -> tl seq = fun xs ->
  match xs with
  | Cons(_,tl) -> tl

type (_,_,_,_) lens =
  | ZeroO  : ('hd0 wrap, 'hd1 wrap, ('hd0 wrap * 'tl) seq, ('hd1 wrap * 'tl) seq) lens
  | ZeroM  : ('hd0 list, 'hd1 list, ('hd0 list * 'tl) seq, ('hd1 list * 'tl) seq) lens
  | Succ : ('a, 'b, 'tl0 seq, 'tl1 seq) lens
           -> ('a,'b, ('hd * 'tl0) seq, ('hd * 'tl1) seq) lens

let rec get : type a b xs ys. (a, b, xs, ys) lens -> xs -> a e = fun ln xs ->
  match ln with
  | ZeroO -> seq_head xs
  | ZeroM -> seq_head xs
  | Succ ln' -> get ln' (seq_tail xs)

let rec put : type a b xs ys. (a,b,xs,ys) lens -> xs -> b e -> ys =
  fun ln xs b ->
  match ln with
  | ZeroO -> Cons(b, seq_tail xs)
  | ZeroM -> Cons(b, seq_tail xs)
  | Succ ln' ->
     begin match xs with
     | Cons(a, xs') -> Cons(a, put ln' xs' b)
     end

type ('a,'b,'xs,'ys) role = {rolename:string; lens:('a,'b,'xs,'ys) lens}

let rec wrap_merge : type s. s wrap -> s wrap -> s wrap = fun l r ->
  match l, r with
  | WrapSend l, WrapSend r -> WrapSend (fun obj -> (l (Some (r obj))))
  | WrapRecv l, WrapRecv r -> WrapRecv (lazy (Event.choose [Lazy.force l; Lazy.force r]))
  | WrapClose, WrapClose -> WrapClose
  | WrapGuard w1, w2 ->
     WrapGuard
       (let rec w =
          (lazy begin
               try
                 let w1 = remove_guards [w] w1 in
                 wrap_merge w1 w2
               with
                 UngardedLoop -> w2
             end)
        in w)
  | w1, ((WrapGuard _) as w2) -> wrap_merge w2 w1
  | WrapSend _, WrapRecv _ -> assert false
  | WrapRecv _, WrapSend _ -> assert false
  | WrapRecv _, WrapClose -> assert false
  | WrapClose, WrapRecv _ -> assert false

let rec e_merge : type t. t e -> t e -> t e = fun l r ->
  match l, r with
  | EOne l, EOne r -> EOne (wrap_merge l r)
  | EList l, EList r -> EList (List.map2 wrap_merge l r)
                           
let rec seq_merge : type t.
    t seq -> t seq -> t seq =
  fun ls rs ->
  match ls, rs with
  | Cons(hd_l,tl_l), Cons(hd_r, tl_r) ->
     Cons(e_merge hd_l hd_r,
          seq_merge tl_l tl_r)
  | Nil, _ -> Nil

let eps {endpoints} = endpoints

let rec goto2 =fun xs ->
  {global=Guard((lazy(Lazy.force xs).global));
   endpoints=
     Cons(EOne(WrapGuard(lazy (unone @@ get ZeroO (eps @@ Lazy.force xs)))),
     Cons(EOne(WrapGuard(lazy (unone @@ get (Succ ZeroO) (eps @@ Lazy.force xs)))),
     Nil))}

let rec goto3 =fun xs ->
  {global=Guard((lazy(Lazy.force xs).global));
   endpoints=
     Cons(EOne(WrapGuard(lazy (unone @@ get ZeroO (eps @@ Lazy.force xs)))),
     Cons(EOne(WrapGuard(lazy (unone @@ get (Succ ZeroO) (eps @@ Lazy.force xs)))),
     Cons(EOne(WrapGuard(lazy (unone @@ get (Succ (Succ ZeroO)) (eps @@ Lazy.force xs)))),
     Nil)))}

let rec goto4 =fun xs ->
  {global=Guard((lazy(Lazy.force xs).global));
   endpoints=
     Cons(EOne(WrapGuard(lazy (unone @@ get ZeroO (eps @@ Lazy.force xs)))),
     Cons(EOne(WrapGuard(lazy (unone @@ get (Succ ZeroO) (eps @@ Lazy.force xs)))),
     Cons(EOne(WrapGuard(lazy (unone @@ get (Succ (Succ ZeroO)) (eps @@ Lazy.force xs)))),
     Cons(EOne(WrapGuard(lazy (unone @@ get (Succ (Succ (Succ ZeroO))) (eps @@ Lazy.force xs)))),
     Nil))))}

let one xs  = Cons(EOne(WrapClose), xs)
let nil = Nil

  
let a = {rolename="A";
         lens=ZeroO}
let b = {rolename="B";
         lens=Succ ZeroO}
let c = {rolename="C";
         lens=Succ (Succ ZeroO)}
let d = {rolename="D";
         lens=Succ (Succ (Succ ZeroO))}
let msg =
  {name="msg";
   make_obj=(fun f -> object method msg=f end);
   call_obj=(fun o -> o#msg);
   make_var=(fun v -> `msg(v))}
let left =
  {name="left";
   make_obj=(fun f -> object method left=f end);
   call_obj=(fun o -> o#left);
   make_var=(fun v -> `left(v))}
let right =
  {name="right";
   make_obj=(fun f -> object method right=f end);
   call_obj=(fun o -> o#right);
   make_var=(fun v -> `right(v))}
let left_or_right =
  {obj_merge=(fun l r -> object method left=l#left method right=r#right end);
   obj_splitL=(fun lr -> (lr :> <left : _>));
   obj_splitR=(fun lr -> (lr :> <right : _>));
  }

(* let finish =
 *   let rec fini = lazy (Cons(Close, fini)) in
 *   Lazy.from_val (Lazy.force fini) *)

let finish2 = {global=Finish; endpoints=one @@ one @@ nil}
let finish3 = {global=Finish; endpoints=one @@ one @@ one @@ nil}
let finish4 = {global=Finish; endpoints=one @@ one @@ one @@ one @@ nil}
let finish5 = {global=Finish; endpoints=one @@ one @@ one @@ one @@ one @@ nil}

module type LIN = sig
  type 'a lin
  val mklin : 'a -> 'a lin
  val unlin : 'a lin -> 'a
end


let map_option f = function
  | Some x -> Some (f x)
  | None -> None

let choice_at : 'ep 'ep_l 'ep_r 'g0_l 'g0_r 'g1 'g2.
  (close wrap, (< .. > as 'ep) wrap, 'g1 seq, 'g2 seq) role ->
  ('ep, < .. > as 'ep_l, < .. > as 'ep_r) obj_merge ->
  ('ep_l wrap, close wrap, 'g0_l seq, 'g1 seq) role * 'g0_l seq prot ->
  ('ep_r wrap, close wrap, 'g0_r seq, 'g1 seq) role * 'g0_r seq prot ->
  'g2 seq prot
  = fun r merge (r',{global=p0;endpoints=g0left}) (r'',{global=p1;endpoints=g0right}) ->
  let epL, epR = unone @@ get r'.lens g0left, unone @@ get r''.lens g0right in
  let g1left, g1right =
    put r'.lens g0left (EOne WrapClose), put r''.lens g0right (EOne WrapClose) in
  let g1 = seq_merge g1left g1right in
  let g2 = put r.lens g1 (EOne (WrapSend (fun obj ->
                     let oleft, oright = unwrap_send epL, unwrap_send epR in
                     merge.obj_merge (oleft (map_option merge.obj_splitL obj)) (oright (map_option merge.obj_splitR obj)))))
  in
  {global=Choice(r.rolename, p0, p1); endpoints=g2}
  
let make_send rB lab (ph: _ placeholder) epA =
  let method_ obj =
    let ph, epA = 
      match obj with
      | None ->
         ignore (force_wrap epA);
         ph, epA
      | Some obj ->
         let ph', epA' = lab.call_obj obj in
         unify ph ph';
         ph, wrap_merge epA epA'
    in
    ph, epA
  in
  (* < lab : v -> epA > *)
  WrapSend (fun obj ->
      lab.make_obj (method_ obj))

let make_recv rA lab ph epB =
  let wrapvar v epB =
    (* [`role_rA of [`lab of v * epB ] ] *)
      lab.make_var (v, epB)
  in
  WrapRecv
    (lazy begin
    ignore (force_wrap epB);
    (Event.wrap
       (Event.guard (fun () -> Event.receive ph.channel (* delay placeholder resolution until actual reception *)))
       (fun v -> wrapvar v (unwrap epB)))
    end)

let ( --> ) : 'roleAVar 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
  ('epA wrap, 'labelobj wrap,             'g1, 'g2) role ->
  ('epB wrap, 'labelvar Event.event wrap, 'g0, 'g1) role ->
  (< .. > as 'labelobj, [> ] as 'labelvar, 'v placeholder * 'epA wrap, 'v * 'epB) label ->
  'g0 prot -> 'g2 prot
  = fun rA rB label {global=p0; endpoints=g0} ->
  let ch = create_placeholder ()
  in
  let epB = get rB.lens g0 in
  let ev  = make_recv rA label ch (unone epB) in
  let g1  = put rB.lens g0 (EOne ev)
  in
  let epA = get rA.lens g1 in
  let obj = make_send rB label ch (unone epA) in
  let g2  = put rA.lens g1 (EOne obj)
  in {global=Seq(Comm,rA.rolename,rB.rolename,label.name, p0); endpoints=g2}

let force_e : type t. t e -> unit = function
  | EOne v -> force_wrap v
  | EList xs -> List.iter force_wrap xs
   
let rec force_all : type t. t seq -> unit = function
  | Cons(hd, tl) -> force_e hd; force_all tl
  | Nil -> ()

let get_ep r {endpoints=g} = force_all g; unwrap (unone (get r.lens g))

let send (ph, cont) v = Event.sync (Event.send ph.channel v); unwrap cont
let receive ev = Event.sync ev
let close Close = ()

let rec remove_guards : type t. t wrap lazy_t list -> t wrap lazy_t -> t wrap = fun acc w ->
  if find_physeq acc w then
    raise UngardedLoop
  else begin match Lazy.force w with
       | WrapGuard w' ->
          remove_guards (w'::acc) w'
       | w -> w
       end
