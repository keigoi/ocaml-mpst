(* type 'a seq = 'a Hlist.Make(State).seq =
  | ( :: ) : 'hd State.t * 'tl seq -> [ `cons of 'hd * 'tl ] seq
  | [] : ([ `cons of unit * 'a ] as 'a) seq

type ('obj, 'ot, 'var, 'vt) label = {
  obj : ('obj, 'ot) Rows.method_;
  var : ('var, 'vt) Rows.constr;
}
(** {b Message labels} for global combinators, which is a pair of a first-class
    method and a {i variant constructor}. A variant constructor is of form
    [(fun x -> `lab(x))], indicating how a variant value is constructed. *)

type ('t, 'u, 'ts, 'us, 'robj, 'mt) role = {
  role_index : ('t, 'u, 'ts, 'us) Hlist.idx;  (** The index of a role. *)
  role_label : ('robj, 'mt) Rows.method_;  (** The label of a role. *)
}

type (_,_) out
type _ inp

type 'a t = <s: <compute : (int, <s: <compute : (int, 
<s: [`result of int * <s: [`result of int *    <s: <stop: (unit, unit) out> > ] inp> ] inp>)     out> >) out> >
as 'a


module Open = struct
  type _ t =
    | [] : ([ `cons of unit * 'a ] as 'a) t
    | ( :: ) : (unit, 'b, 'bb, 'cc, _, _) role * 'bb t -> 'cc t
end

module type STATE = sig
  type 'a t
  type 'var inp
  type 'k out

  val unit : unit t
  val merge : 'a t -> 'a t -> 'a t
end

module Global (State : STATE) = struct
  type 't global =
    | Comm :
        ('a, 'b, 'c, 'd, 'e, 'f State.inp) role
        * ('g, 'e, 'h, 'c, 'b, 'i) role
        * ('i, 'a State.out, 'f, 'g) label
        * 'h global
        -> 'd global
    | ChoiceAt :
        ('a, 'b, 'c, 'd, 'e, 'f) role
        * ('b, 'g, 'h) Rows.disj
        * (('g, unit, 'i, 'c, 'j, 'k) role * 'i global)
        * (('h, unit, 'l, 'c, 'm, 'n) role * 'l global)
        -> 'd global
    | PartialFinish : 'b Open.t * 'b global -> 'b global
    | Finish : ([ `cons of unit * 'a ] as 'a) global

  module Sessions = Hlist.Make (State)

  type 't seq = 't Sessions.seq

  let rec merge_seq : type t. t seq -> t seq -> t seq =
   fun ls rs ->
    match (ls, rs) with
    | Sessions.[], _ -> []
    | _, [] -> []
    | l :: ls, r :: rs -> State.merge l r :: merge_seq ls rs

  let rec eval : type t. t global -> t seq =
    let open Sessions in
    fun g ->
      match g with
      | Comm (ra, rb, lab, g) ->
          let key = Name.make () in
          let g = eval g in
          let b = seq_get rb.role_index g in
          let g =
            seq_put rb.role_index g (State.inp ra.role_label lab.var key b)
          in
          let a = seq_get ra.role_index g in
          let g =
            seq_put ra.role_index g (State.out rb.role_label lab.obj key a)
          in
          g
      | ChoiceAt (ra, disj, (ra1, g1), (ra2, g2)) ->
          let g1 = eval g1 in
          let g2 = eval g2 in
          let a1 = seq_get ra1.role_index g1
          and a2 = seq_get ra2.role_index g2 in
          let g1 = seq_put ra1.role_index g1 State.unit
          and g2 = seq_put ra2.role_index g2 State.unit in
          let g = merge_seq g1 g2 in
          let a = State.internal_choice disj a1 a2 in
          seq_put ra.role_index g a
      | PartialFinish (keep_idxs, keeps) ->
          let rec finish :
              type b. keep_idxs:b Open.t -> keeps:b seq lazy_t -> b seq =
           fun ~keep_idxs ~keeps ->
            match keep_idxs with
            | Open.[] -> Sessions.[]
            | idx :: rest_idxs ->
                let state =
                  (* get the state of the involved role *)
                  State.lazy_
                    (lazy (seq_get2 idx.role_index (Lazy.force keeps)))
                and rest =
                  (* rest of the states *)
                  lazy (seq_put2 idx.role_index (Lazy.force keeps) State.unit)
                in
                let states = finish ~keep_idxs:rest_idxs ~keeps:rest in
                (* put the state and return it *)
                seq_put idx.role_index states state
          in
          finish ~keep_idxs ~keeps:(lazy (eval keeps))
      | Finish -> []
end *)
