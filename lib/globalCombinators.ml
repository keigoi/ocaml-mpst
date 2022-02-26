type ('obj, 'ot, 'var, 'vt) label = {
  obj : ('obj, 'ot) Rows.method_;
  var : ('var, 'vt) Rows.constr;
}

type ('t, 'u, 'ts, 'us, 'robj, 'mt) role = {
  role_index : ('t, 'u, 'ts, 'us) Hlist.idx;
  role_label : ('robj, 'mt) Rows.method_;
}

module Open = struct
  type _ t =
    | [] : ([ `cons of unit * 'a ] as 'a) t
    | ( :: ) : (unit, 'b, 'bb, 'cc, _, _) role * 'bb t -> 'cc t
end

module Sessions = Hlist.Make (State)
open Sessions

type 'a seq = 'a Hlist.Make(State).seq =
  | ( :: ) : 'hd State.t * 'tl seq -> [ `cons of 'hd * 'tl ] seq
  | [] : ([ `cons of unit * 'a ] as 'a) seq

let rec merge_seq : type t. t seq -> t seq -> t seq =
 fun ls rs ->
  match (ls, rs) with
  | Sessions.[], _ -> []
  | _, [] -> []
  | l :: ls, r :: rs -> State.merge l r :: merge_seq ls rs

type ('env, 't) global = 'env -> 't seq

let choice_at ra disj (ra1, g1) (ra2, g2) ctx =
  let g1 = g1 ctx and g2 = g2 ctx in
  let a1 = seq_get ra1.role_index g1 and a2 = seq_get ra2.role_index g2 in
  let g1 = seq_put ra1.role_index g1 State.unit
  and g2 = seq_put ra2.role_index g2 State.unit in
  let g = merge_seq g1 g2 in
  let a = State.internal_choice disj a1 a2 in
  seq_put ra.role_index g a

let partial_finish keep_idxs g =
  let rec make_partial_finish :
      type b. keep_idxs:b Open.t -> keeps:b seq lazy_t -> b seq =
   fun ~keep_idxs ~keeps ->
    match keep_idxs with
    | Open.[] -> Sessions.[]
    | idx :: rest_idxs ->
        let state =
          (* get the state of the involved role *)
          State.loop (lazy (seq_get2 idx.role_index (Lazy.force keeps)))
        and rest =
          (* rest of the states *)
          lazy (seq_put2 idx.role_index (Lazy.force keeps) State.unit)
        in
        let states = make_partial_finish ~keep_idxs:rest_idxs ~keeps:rest in
        (* put the state and return it *)
        seq_put idx.role_index states state
  in
  make_partial_finish ~keep_idxs ~keeps:g

let fix_with keep_idxs f ctx =
  let rec self =
    lazy (partial_finish keep_idxs (lazy (f (fun _ctx -> Lazy.force self) ctx)))
  in
  Lazy.force self

let finish _ = Sessions.[]

let rec extract_seq : type u. u Sessions.seq -> u = function
  | Sessions.[] ->
      let rec nil = `cons ((), nil) in
      nil
  | st :: tail ->
      let hd = State.determinise st in
      let tl = extract_seq tail in
      `cons (hd, tl)

let extract g env = extract_seq (g env)

module type C = sig
  type chan
  type 'a name
  type _ out
  type 'var inp

  val make : unit -> chan
  val new_name : chan -> 'a name
  val select : 'a out -> 'a
  val branch : 'a inp -> 'a

  val out :
    ('a, 'b) Rows.method_ ->
    ('b, 'c out) Rows.method_ ->
    int name ->
    'c State.t ->
    'a State.t

  val inp :
    ('a, 'b inp) Rows.method_ ->
    ('b, 'c) Rows.constr ->
    int name ->
    'c State.t ->
    'a State.t
end

module type S = sig
  type 's out
  type 's inp
  type chan

  val select : 'a out -> 'a
  val branch : 'a inp -> 'a
  val close : unit -> unit

  type env

  val ( --> ) :
    ('a, 'b, 'c, 'd, 'e, 'f inp) role ->
    ('g, 'e, 'h, 'c, 'b, 'i) role ->
    ('i, 'a out, 'f, 'g) label ->
    (env, 'h) global ->
    (env, 'd) global

  val make_env : unit -> env
end

module Make (Chan : C) = struct
  type 's out = 's Chan.out
  type 's inp = 's Chan.inp

  exception UnguardedLoop = State.UnguardedLoop

  let select = Chan.select
  let branch = Chan.branch
  let close () = ()

  type env = < chan : string * string -> Chan.chan >

  let ( --> ) ra rb lab g ctx =
    let g = g ctx in
    let ch = ctx#chan (ra.role_label.method_name, rb.role_label.method_name) in
    let key = Chan.new_name ch in
    let b = seq_get rb.role_index g in
    let g = seq_put rb.role_index g (Chan.inp ra.role_label lab.var key b) in
    let a = seq_get ra.role_index g in
    let g = seq_put ra.role_index g (Chan.out rb.role_label lab.obj key a) in
    g

  type t = (string * string, Chan.chan) Hashtbl.t

  let lookup (t : t) (key : string * string) =
    match Hashtbl.find_opt t key with
    | Some ch -> ch
    | None ->
        let ch = Chan.make () in
        Hashtbl.add t key ch;
        ch

  let make_env () =
    let t = Hashtbl.create 42 in
    object
      method chan key = lookup t key
    end
end

module Sync = struct
  include Chvec.Sync
  include Make (Chvec.Sync)
end

module Async = struct
  include Chvec.Async
  include Make (Chvec.Async)

  let extract_ = extract
  let extract g = extract_ g (make_env ())
end

include Async
