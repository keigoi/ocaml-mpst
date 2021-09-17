open Types
open Chvecs

exception UnguardedLoop = State.UnguardedLoop

let (-->) ri rj lab cont =
let name = Name.make () in
let tj = Seq.get rj.role_index cont in
let tj' =
  ri.role_label.make_obj @@ (WrappedState.make lab.var name tj : _ inp)
in
let tj' =
  State.make
    (merge_inp ri.role_label)
    (merge_inp_next ri.role_label)
    tj'
in
let mid = Seq.put rj.role_index cont tj' in
let ti = Seq.get ri.role_index mid in
let ti' =
  rj.role_label.make_obj @@
    lab.obj.make_obj ((name, ti): _ out)
in
let ti' =
  State.make
    (merge_out rj.role_label lab.obj)
    (merge_out_next rj.role_label lab.obj)
    ti'
in
let curr = Seq.put ri.role_index mid ti' in
curr

let choice_at r disj (r', left) (r'', right) =
let sl = Seq.get r'.role_index left
and sr = Seq.get r''.role_index right
and left = Seq.put r'.role_index left Seq.closed
and right = Seq.put r''.role_index right Seq.closed
in
let mid = Seq.merge left right in
Seq.put r.role_index mid (State.make_internal_choice sl sr disj)

type (_,_) upd =
| [] : ('aa, 'aa) upd
| (::) : ('a,'b,'aa,'bb,_,_) role * ('bb,'cc) upd -> ('aa,'cc) upd

(** shadowing upd above *)
type 'a list = 'a Stdlib.List.t =
| [] 
| (::) of 'a * 'a list

let rec put_unbound : type aa bb. aa Seq.t -> (aa,bb) upd -> bb Seq.t =
fun seq upd ->
match upd with
| [] -> 
  seq
| role::upd -> 
  put_unbound (Seq.put role.role_index seq (State.make_unbound ())) upd

let finish = Seq.SeqNil

let rec tying_unbound : type u. u Seq.t -> u Seq.t -> unit = fun self g ->
match self with
| SeqNil -> ()
| SeqCons(st,self) -> 
  State.bind_state ~to_:st ~from:(Seq.head g);
  tying_unbound self (Seq.tail g)

let fix_with upd f =
let self = put_unbound SeqNil upd in
let g = f self in
tying_unbound self g;
g

let rec extract : type u. u Seq.t -> u = function
| SeqCons(st,tail) ->
  let hd = State.determinise ~dict:StateHash.empty st in
  let tl = extract tail in
  `cons(hd, tl)
| SeqNil ->
  let rec nil = `cons((),nil) in nil