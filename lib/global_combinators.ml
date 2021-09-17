open Types
open States
open Wrapped
open Names
open Seq
open Channel_vectors

exception UnguardedLoop = States.UnguardedLoop

let (-->) ri rj lab cont =
let name = ref @@ Name (Event.new_channel ()) in
let tj = get rj.role_index cont in
let tj' = 
  ri.role_label.make_obj @@ (make_wrapped lab.var name tj : _ inp)
in
let tj' =
  make_state
    (merge_inp ri.role_label)
    (merge_inp_next ri.role_label)
    tj'
in
let mid = put rj.role_index cont tj' in
let ti = get ri.role_index mid in
let ti' =
  rj.role_label.make_obj @@
    lab.obj.make_obj ((name, ti): _ out)
in
let ti' =
  make_state 
    (merge_out rj.role_label lab.obj)
    (merge_out_next rj.role_label lab.obj)
    ti'
in
let curr = put ri.role_index mid ti' in
curr

let choice_at r disj (r', left) (r'', right) =
let sl = get r'.role_index left
and sr = get r''.role_index right
and left = put r'.role_index left closed
and right = put r''.role_index right closed
in
let mid = seq_merge left right in
put r.role_index mid (make_internal_choice sl sr disj)

type (_,_) upd =
| [] : ('aa, 'aa) upd
| (::) : ('a,'b,'aa,'bb,_,_) role * ('bb,'cc) upd -> ('aa,'cc) upd

(** shadowing upd above *)
type 'a list = 'a Stdlib.List.t =
| [] 
| (::) of 'a * 'a list

let rec put_unbound : type aa bb. aa t -> (aa,bb) upd -> bb t =
fun seq upd ->
match upd with
| [] -> 
  seq
| role::upd -> 
  put_unbound (put role.role_index seq (make_unbound_state ())) upd

let finish = SeqNil

let rec tying_unbound : type u. u t -> u t -> unit = fun self g ->
match self with
| SeqNil -> ()
| SeqCons(st,self) -> 
  States.bind_state ~to_:st ~from:(seq_head g);
  tying_unbound self (seq_tail g)

let fix_with upd f =
let self = put_unbound SeqNil upd in
let g = f self in
tying_unbound self g;
g

let rec extract : type u. u t -> u = function
| SeqCons(st,tail) ->
  let hd = determinise ~dict:StateHash.empty st in
  let tl = extract tail in
  `cons(hd, tl)
| SeqNil ->
  let rec nil = `cons((),nil) in nil