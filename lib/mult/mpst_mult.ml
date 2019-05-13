
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

type ('a,'loop) prot = {global:Mpst_base.Ast.global; endpoints:'a; loop:'loop}
let print_global {global} = Mpst_base.Ast.print_global global

type 'a placeholder =
  {mutable channel: 'a Event.channel}

let create_placeholder () =
  let channel = Event.new_channel () in
  {channel}

let id x = x

let unify p q = q.channel <- p.channel
let unify_list ps qs = List.map2 (fun p q -> q.channel <- p.channel) ps qs

type _ wrap =
  | WrapSend : ((< .. > as 'obj) option -> 'obj) -> 'obj wrap
  | WrapRecv : ((< .. > as 'obj) option -> 'obj) -> 'obj wrap
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
  | WrapRecv(obj) -> obj None
  | WrapClose -> Close
  | WrapGuard w ->
     let w = remove_guards [] w in
     unwrap w

let rec force_wrap : type t. t wrap -> unit = function
  | WrapSend(obj) -> ignore @@ obj None
  | WrapRecv(obj) -> ignore @@ obj None
  | WrapClose -> ()
  | WrapGuard w -> ignore (remove_guards [] w)

let rec unwrap_send : 'a. (< .. > as 'a) wrap -> 'a option -> 'a = function
  | WrapSend(obj) -> obj
  | WrapGuard(w) ->
     let w = remove_guards [] w in
     unwrap_send w
  | _ -> assert false

type _ e =
  | EOne : 'a wrap -> 'a wrap e
  | EList : 'a wrap list -> 'a list e

let unone (EOne v) = v
let unlist (EList xs) = xs
let force_e : type t. t e -> unit = function
  | EOne t -> force_wrap t
  | EList xs -> List.iter force_wrap xs

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

type ('robj,'rvar,'c,'a,'b,'xs,'ys) role = {label:('robj,'rvar,'c,'c) label; lens:('a,'b,'xs,'ys) lens}

let rec wrap_merge : type s. s wrap -> s wrap -> s wrap = fun l r ->
  match l, r with
  | WrapSend l, WrapSend r -> WrapSend (fun obj -> (l (Some (r obj))))
  | WrapRecv l, WrapRecv r -> WrapRecv (fun obj -> (l (Some (r obj))))
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

type loop = [`Loop]
type finish = [`Finish]

type (_,_) arr =
  Arr : (('g2 seq,'loop2) prot -> ('g1 seq,'loop1) prot) -> ('g1 * 'loop1, 'g2 * 'loop2) arr

let (>>) (Arr f) (Arr g) = Arr (fun x -> f (g x))

let force_arr (lazy (Arr f)) = f
let run_arr_ (Arr f) x = f x
let run_arr_finish (Arr f) (x:(_,finish) prot) = f x


let rec goto2_ =fun xs ->
  {global=Guard((lazy(Lazy.force xs).global));
   loop=`Loop;
   endpoints=
     Cons(EOne(WrapGuard(lazy (unone @@ get ZeroO (eps @@ Lazy.force xs)))),
     Cons(EOne(WrapGuard(lazy (unone @@ get (Succ ZeroO) (eps @@ Lazy.force xs)))),
     Nil))}

let rec goto3_ =fun xs ->
  {global=Guard((lazy(Lazy.force xs).global));
   loop=`Loop;
   endpoints=
     Cons(EOne(WrapGuard(lazy (unone @@ get ZeroO (eps @@ Lazy.force xs)))),
     Cons(EOne(WrapGuard(lazy (unone @@ get (Succ ZeroO) (eps @@ Lazy.force xs)))),
     Cons(EOne(WrapGuard(lazy (unone @@ get (Succ (Succ ZeroO)) (eps @@ Lazy.force xs)))),
     Nil)))}

let rec goto4_ =fun xs ->
  {global=Guard((lazy(Lazy.force xs).global));
   loop=`Loop;
   endpoints=
     Cons(EOne(WrapGuard(lazy (unone @@ get ZeroO (eps @@ Lazy.force xs)))),
     Cons(EOne(WrapGuard(lazy (unone @@ get (Succ ZeroO) (eps @@ Lazy.force xs)))),
     Cons(EOne(WrapGuard(lazy (unone @@ get (Succ (Succ ZeroO)) (eps @@ Lazy.force xs)))),
     Cons(EOne(WrapGuard(lazy (unone @@ get (Succ (Succ (Succ ZeroO))) (eps @@ Lazy.force xs)))),
     Nil))))}

let loop_ goto = fun body ->
  Arr (begin fun g2 ->
    let rec r =
      lazy begin
          run_arr_ (body (Arr (fun _ -> goto r))) g2
        end
    in
    Lazy.force r
    end)

let loop2 x = loop_ goto2_ x
let loop3 x = loop_ goto3_ x
let loop4 x = loop_ goto4_ x

let one xs  = Cons(EOne(WrapClose), xs)
let list n xs  = Cons(EList(List.init n (fun _ ->WrapClose)), xs)
let nil = Nil

  
let a = {label={name="A";
                make_obj=(fun v->object method role_A=v end);
                call_obj=(fun o->o#role_A);
               make_var=(fun v->(`role_A(v):[`role_A of _]))}; (* explicit annotataion is mandatory *)
         lens=ZeroO}
let b = {label={name="B";
                make_obj=(fun v->object method role_B=v end);
                call_obj=(fun o->o#role_B);
               make_var=(fun v->(`role_B(v):[`role_B of _]))}; (* explicit annotataion is mandatory *)
         lens=Succ ZeroO}
let c = {label={name="C";
                make_obj=(fun v->object method role_C=v end);
                call_obj=(fun o->o#role_C);
               make_var=(fun v->(`role_C(v):[`role_C of _]))}; (* explicit annotataion is mandatory *)
         lens=Succ (Succ ZeroO)}
let d = {label={name="D";
                make_obj=(fun v->object method role_D=v end);
                call_obj=(fun o->o#role_D);
                make_var=(fun v->(`role_D(v):[`role_D of _]))}; (* explicit annotataion is mandatory *)
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
let to_b m =
  {obj_merge=(fun l r -> object method role_B=m.obj_merge l#role_B r#role_B end);
   obj_splitL=(fun lr -> object method role_B=m.obj_splitL lr#role_B end);
   obj_splitR=(fun lr -> object method role_B=m.obj_splitR lr#role_B end);
  }
let b_or_c =
  {obj_merge=(fun l r -> object method role_B=l#role_B method role_C=r#role_C end);
   obj_splitL=(fun lr -> (lr :> <role_B : _>));
   obj_splitR=(fun lr -> (lr :> <role_C : _>));
  }

(* let finish =
 *   let rec fini = lazy (Cons(Close, fini)) in
 *   Lazy.from_val (Lazy.force fini) *)

let finish2 = {global=Finish; loop=`Finish; endpoints=one @@ one @@ nil}
let finish3 = {global=Finish; loop=`Finish; endpoints=one @@ one @@ one @@ nil}
let finish4 = {global=Finish; loop=`Finish; endpoints=one @@ one @@ one @@ one @@ nil}
let finish5 = {global=Finish; loop=`Finish; endpoints=one @@ one @@ one @@ one @@ one @@ nil}

module type LIN = sig
  type 'a lin
  val mklin : 'a -> 'a lin
  val unlin : 'a lin -> 'a
end


let map_option f = function
  | Some x -> Some (f x)
  | None -> None

let choice_at : 'ep 'ep_l 'ep_r 'g0_l 'g0_r 'g1 'g2 'g3 'loop 'loop_l 'loop_r.
  (_, _, _, close wrap, (< .. > as 'ep) wrap, 'g1 seq, 'g2 seq) role ->
  ('ep, < .. > as 'ep_l, < .. > as 'ep_r) obj_merge ->
  (_, _, _, 'ep_l wrap, close wrap, 'g0_l seq, 'g1 seq) role * ('g0_l * 'loop_l,'g3 * 'loop) arr ->
  (_, _, _, 'ep_r wrap, close wrap, 'g0_r seq, 'g1 seq) role * ('g0_r * 'loop_r,'g3 * 'loop) arr ->
  ('g2 * 'loop,'g3 * 'loop) arr
  = fun r merge (r',Arr (left)) (r'',Arr (right)) ->
  Arr (begin fun cont ->
  let {global=p0;endpoints=g0left;loop=_} = left cont in
  let {global=p1;endpoints=g0right;loop=_} = right cont in
  let (EOne epL), (EOne epR) = get r'.lens g0left, get r''.lens g0right (* FIXME *) in
  let g1left, g1right =
    put r'.lens g0left (EOne WrapClose), put r''.lens g0right (EOne WrapClose) in
  let g1 = seq_merge g1left g1right in
  let g2 = put r.lens g1 (EOne (WrapSend (fun obj ->
                     let oleft, oright = unwrap_send epL, unwrap_send epR in
                     merge.obj_merge (oleft (map_option merge.obj_splitL obj)) (oright (map_option merge.obj_splitR obj)))))
  in
  {global=Choice(r.label.name, p0, p1); loop=cont.loop; endpoints=g2}
  end)
  

let make_send unify_ wrap unwrap rB lab ph epA =
  let method_ obj =
    let ph, epA = 
      match obj with
      | None ->
         ignore (force_wrap epA);
         ph, epA
      | Some obj ->
         let ph', epA' = unwrap (lab.call_obj (rB.label.call_obj obj)) in
         unify_ ph ph';
         ph, wrap_merge epA epA'
    in
    wrap (ph, epA)
  in
  (* <role_rB : < lab : v -> epA > > *)
  WrapSend (fun obj ->
      rB.label.make_obj (lab.make_obj (method_ obj)))

let make_recv receive_ rA lab ph epB =
  let method_ obj =
    let ev =
      Event.wrap
        (Event.guard (fun () -> receive_ ph))
        (fun v -> lab.make_var (v, (unwrap epB)))
    in
      match obj with
      | None ->
         force_wrap epB;
         ev
      | Some obj ->
         Event.choose [ev; rA.label.call_obj obj]
  in
  WrapRecv (fun obj ->
      rA.label.make_obj (method_ obj))

let ( --> ) : 'roleAVar 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
  (< .. > as 'roleAobj, _, 'labelvar Event.event, 'epA wrap, 'roleBobj wrap, 'g1 seq, 'g2 seq) role ->
  (< .. > as 'roleBobj, _, 'labelobj,             'epB wrap, 'roleAobj wrap, 'g0 seq, 'g1 seq) role ->
  (< .. > as 'labelobj, [> ] as 'labelvar, 'v placeholder * 'epA wrap, 'v * 'epB) label ->
  ('g2 * 'loop,'g0 * 'loop) arr
  = fun rA rB label ->
  Arr begin fun cont ->
  let {global=p0; endpoints=g0} = cont in
  let ph = create_placeholder ()
  in
  let EOne epB = get rB.lens g0 in
  let ev  = make_recv (fun {channel} -> Event.receive channel) rA label ph epB in
  let g1  = put rB.lens g0 (EOne ev)
  in
  let EOne epA = get rA.lens g1 in
  let obj = make_send unify id id rB label ph epA in
  let g2  = put rA.lens g1 (EOne obj)
  in {global=Seq(Comm,rA.label.name,rB.label.name,label.name, p0); loop=cont.loop; endpoints=g2}
  end
  
(* let scatter : 'roleAVar 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
 *   (_,  [>  ] as 'roleAvar, 'labelvar, 'epA wrap, 'roleBobj wrap,             'g1 seq, 'g2 seq) role ->
 *   (< .. > as 'roleBobj, _, 'labelobj, 'epB list, 'roleAvar Event.event list, 'g0 seq, 'g1 seq) role ->
 *   (< .. > as 'labelobj, [> ] as 'labelvar, 'v placeholder list * 'epA wrap, 'v * 'epB) label ->
 *   ('g2 * 'loop,'g0 * 'loop) arr
 *   = fun rA rB label ->
 *   Arr begin fun cont ->
 *   let {global=p0; endpoints=g0} = cont in
 *   let EList epBs = get rB.lens g0 in
 *   let chs = List.map (fun _ -> create_placeholder ()) epBs
 *   in
 *   let evs  = List.map2 (make_recv (fun {channel} -> Event.receive channel) rA label) chs epBs in
 *   let g1  = put rB.lens g0 (EList evs)
 *   in
 *   let EOne epA = get rA.lens g1 in
 *   let obj = make_send unify_list id id rB label chs epA in
 *   let g2  = put rA.lens g1 (EOne obj)
 *   in {global=Seq(Scatter,rA.label.name,rB.label.name,label.name, p0); endpoints=g2; loop=cont.loop}
 *   end *)

let receive_list = function
  | ch::chs -> Event.wrap (Event.receive ch) (fun v -> v :: List.map (fun ch -> Event.sync (Event.receive ch)) chs)
  | [] -> failwith "no channel"
  
(* let gather : 'roleAVar 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
 *   (_,  [>  ] as 'roleAvar, 'labelvar, 'epA list, 'roleBobj list,             'g1 seq, 'g2 seq) role ->
 *   (< .. > as 'roleBobj, _, 'labelobj, 'epB wrap, 'roleAvar Event.event wrap, 'g0 seq, 'g1 seq) role ->
 *   (< .. > as 'labelobj, [> ] as 'labelvar, 'v placeholder * 'epA wrap, 'v list * 'epB) label ->
 *   ('g2 * 'loop,'g0 * 'loop) arr
 *   = fun rA rB label ->
 *   Arr begin fun cont ->
 *   let {global=p0; endpoints=g0} = cont in
 *   let EOne epB = get rB.lens g0 in
 *   let rec ev  = lazy (make_recv (fun phs -> let phs = Lazy.force phs in receive_list (List.map (fun ph->ph.channel) phs)) rA label phs epB)
 *   and g1  = lazy (put rB.lens g0 (EOne (Lazy.force ev)))
 *   and epAs = lazy (get rA.lens (Lazy.force g1)) 
 *   and phs = lazy (List.map (fun _ -> create_placeholder ()) (unlist (Lazy.force epAs)))
 *   in
 *   let g1 = Lazy.force g1 and epAs = Lazy.force epAs and chs = Lazy.force phs in
 *   let obj = List.map2 (make_send unify id id rB label) chs (unlist epAs) in
 *   let g2  = put rA.lens g1 (EList obj)
 *   in {global=Seq(Gather,rA.label.name,rB.label.name,label.name, p0); endpoints=g2; loop=cont.loop}
 *   end *)

type 'a oneof = Oneof of 'a

(* let select_one : 'roleAVar 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
 *   (_,  [>  ] as 'roleAvar, 'labelvar, 'epA wrap, 'roleBobj wrap,             'g1 seq, 'g2 seq) role ->
 *   (< .. > as 'roleBobj, _, 'labelobj, 'epB list, 'roleAvar Event.event list, 'g0 seq, 'g1 seq) role ->
 *   (< .. > as 'labelobj, [> ] as 'labelvar, 'v placeholder list oneof * 'epA wrap, 'v * 'epB) label ->
 *   ('g2 * loop,'g0 * loop) arr
 *   = fun rA rB label ->
 *   Arr begin fun cont ->
 *   let {global=p0; endpoints=g0} = cont in
 *   let EList epBs = get rB.lens g0 in
 *   let chs = List.map (fun _ -> create_placeholder ()) epBs
 *   in
 *   let evs  = List.map2 (make_recv (fun {channel} -> Event.receive channel) rA label) chs epBs in
 *   let g1  = put rB.lens g0 (EList evs)
 *   in
 *   let EOne epA = get rA.lens g1 in
 *   let obj = make_send unify_list (fun (x,y) -> Oneof(x),y) (fun (Oneof(x),y) -> x,y) rB label chs epA in
 *   let g2  = put rA.lens g1 (EOne obj)
 *   in {global=Seq(SelectOne,rA.label.name,rB.label.name,label.name, p0); endpoints=g2; loop=cont.loop}
 *   end *)
  
(* let offer_one : 'roleAVar 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
 *   (_,  [>  ] as 'roleAvar, 'labelvar, 'epA list, 'roleBobj list,             'g1 seq, 'g2 seq) role ->
 *   (< .. > as 'roleBobj, _, 'labelobj, 'epB wrap, 'roleAvar Event.event wrap, 'g0 seq, 'g1 seq) role ->
 *   (< .. > as 'labelobj, [> ] as 'labelvar, 'v placeholder * 'epA wrap, 'v * 'epB) label ->
 *   ('g2 * 'loop,'g0 * 'loop) arr
 *   = fun rA rB label ->
 *   Arr begin fun cont ->
 *   let {global=p0; endpoints=g0} = cont in
 *   let EOne epB = get rB.lens g0 in
 *   let rec ev  = lazy (make_recv (fun phs -> Event.choose @@ List.map (fun ph -> Event.receive ph.channel) (Lazy.force phs)) rA label phs epB)
 *   and g1  = lazy (put rB.lens g0 (EOne (Lazy.force ev)))
 *   and epAs = lazy (get rA.lens (Lazy.force g1)) 
 *   and phs = lazy (List.map (fun _ -> create_placeholder ()) (unlist (Lazy.force epAs)))
 *   in
 *   let g1 = Lazy.force g1 and epAs = Lazy.force epAs and chs = Lazy.force phs in
 *   let obj = List.map2 (make_send unify id id rB label) chs (unlist epAs) in
 *   let g2  = put rA.lens g1 (EList obj)
 *   in {global=Seq(OfferOne,rA.label.name,rB.label.name,label.name, p0); endpoints=g2; loop=cont.loop}
 *   end *)

let rec force_all : type t. t seq -> unit = function
  | Cons(hd, tl) -> force_e hd; force_all tl
  | Nil -> ()

let unone : type t. t wrap e -> t wrap = function
  | EOne v -> v

let get_ep r {endpoints=g} = force_all g; unwrap (unone (get r.lens g))
let get_ep_list r {endpoints=g} = force_all g; List.map unwrap (unlist (get r.lens g))

let send (ph, cont) v = Event.sync (Event.send ph.channel v); unwrap cont
let sendmany (phs, cont) f =
  List.iteri (fun i ph -> Event.sync (Event.send ph.channel (f i))) phs;
  unwrap cont
let sendone (Oneof(phs), cont) i v =
  Event.sync (Event.send (List.nth phs i).channel v);
  unwrap cont
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


(* let g () =
 *   loop2 (fun self0 -> loop2 (fun self ->
 *       choice_at a (to_b left_or_right)
 *         (a, (a -+-> b) left >> self)
 *         (a, (a --> b) right >> self) >> self0))
 * 
 * let f () =
 *     run_arr_finish (g ()) finish2 *)
