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

exception UnguardedLoop

type ('env, 't) global

val choice_at :
  ('a, 'b, 'c, 'd, 'e, 'f) role ->
  ('b, 'g, 'h) Rows.disj ->
  ('g, unit, 'i, 'c, 'j, 'k) role * ('env, 'i) global ->
  ('h, unit, 'l, 'c, 'm, 'n) role * ('env, 'l) global ->
  ('env, 'd) global

module Open : sig
  type _ t =
    | [] : ([ `cons of unit * 'a ] as 'a) t
    | ( :: ) : (unit, 'b, 'bb, 'cc, 'a, 'c) role * 'bb t -> 'cc t
end

val fix_with :
  'a Open.t -> (('env, 'a) global -> ('env, 'a) global) -> ('env, 'a) global

val finish : ('env, ([ `cons of unit * 'a ] as 'a)) global

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

module Make (X : C) : S with type chan := X.chan
include S with type chan = DynChan.chan

val extract_ : ('t, 'u) global -> 't -> 'u
val extract : (env, 'u) global -> 'u
