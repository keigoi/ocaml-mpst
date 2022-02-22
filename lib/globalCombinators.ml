open Types

exception UnguardedLoop = State.UnguardedLoop

module Sessions = Hlist.Make (State)

type 'a inp = 'a Chvec.inp
type 'a out = 'a Chvec.out
type 'a t = 'a Seq.t

let rec session_merge :
    type x. x Sessions.seq -> x Sessions.seq -> x Sessions.seq =
  let open Sessions in
  fun l r ->
    match (l, r) with
    | _ :: _, _ ->
        let hd = State.merge (seq_head l) (seq_head r) in
        let tl = session_merge (seq_tail l) (seq_tail r) in
        hd :: tl
    | _, _ :: _ -> session_merge r l
    | [], [] -> []

let ( --> ) ri rj lab cont =
  let name = Name.make () in
  let tj = Sessions.seq_get rj.role_index cont in
  let tj' =
    ri.role_label.make_obj @@ (WrappedState.make lab.var name tj : _ inp)
  in
  let tj' =
    State.make
      (Chvec.merge_inp ri.role_label)
      (Chvec.merge_inp_next ri.role_label)
      tj'
  in
  let mid = Sessions.seq_put rj.role_index cont tj' in
  let ti = Sessions.seq_get ri.role_index mid in
  let ti' = rj.role_label.make_obj @@ lab.obj.make_obj ((name, ti) : _ out) in
  let ti' =
    State.make
      (Chvec.merge_out rj.role_label lab.obj)
      (Chvec.merge_out_next rj.role_label lab.obj)
      ti'
  in
  let curr = Sessions.seq_put ri.role_index mid ti' in
  curr

let choice_at r disj (r', left) (r'', right) =
  let sl = Sessions.seq_get r'.role_index left
  and sr = Sessions.seq_get r''.role_index right
  and left = Sessions.seq_put r'.role_index left State.unit
  and right = Sessions.seq_put r''.role_index right State.unit in
  let mid = session_merge left right in
  Sessions.seq_put r.role_index mid (State.make_internal_choice sl sr disj)

type (_, _) upd =
  | [] : ('aa, 'aa) upd
  | ( :: ) : ('a, 'b, 'aa, 'bb, _, _) role * ('bb, 'cc) upd -> ('aa, 'cc) upd

(** shadowing upd above *)
type 'a list = 'a Stdlib.List.t = [] | ( :: ) of 'a * 'a list

let rec put_unbound :
    type aa bb. aa Sessions.seq -> (aa, bb) upd -> bb Sessions.seq =
 fun seq upd ->
  match upd with
  | [] -> seq
  | role :: upd ->
      put_unbound
        (Sessions.seq_put role.role_index seq (State.make_unbound ()))
        upd

let finish = Sessions.Hetero.[]

let rec tying_unbound : type u. u Sessions.seq -> u Sessions.seq -> unit =
 fun self g ->
  match self with
  | Sessions.[] -> ()
  | st :: self ->
      State.bind_state ~to_:st ~from:(Sessions.seq_head g);
      tying_unbound self (Sessions.seq_tail g)

let fix_with upd f =
  let self = put_unbound Sessions.[] upd in
  let g = f self in
  tying_unbound self g;
  g

let rec extract : type u. u Sessions.seq -> u = function
  | Sessions.[] ->
      let rec nil = `cons ((), nil) in
      nil
  | st :: tail ->
      let hd = State.determinise ~cache:StateHash.empty st in
      let tl = extract tail in
      `cons (hd, tl)

let branch = Chvec.branch
let select = Chvec.select
let close = Chvec.close
