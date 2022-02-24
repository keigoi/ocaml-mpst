type 's out
type 's inp

type 'a seq = 'a Hlist.Make(State).seq =
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
(** The {b role type} for global combinators. *)

exception UnguardedLoop of string

val select : 'a out -> 'a
val branch : 'a inp -> 'a
val close : unit -> unit

type 't global

val ( --> ) :
  ('a, 'b, 'c, 'd, 'e, 'f inp) role ->
  ('g, 'e, 'h, 'c, 'b, 'i) role ->
  ('i, 'a out, 'f, 'g) label ->
  'h global ->
  'd global

val choice_at :
  ('a, 'b, 'c, 'd, 'e, 'f) role ->
  ('b, 'g, 'h) Rows.disj ->
  ('g, unit, 'i, 'c, 'j, 'k) role * 'i global ->
  ('h, unit, 'l, 'c, 'm, 'n) role * 'l global ->
  'd global

module Open : sig
  type _ t =
    | [] : ([ `cons of unit * 'a ] as 'a) t
    | ( :: ) : (unit, 'b, 'bb, 'cc, 'a, 'c) role * 'bb t -> 'cc t
end

val fix_with : 'a Open.t -> ('a global -> 'a global) -> 'a global
val finish : ([ `cons of unit * 'a ] as 'a) global
val extract : 'u global -> 'u
