open GlobalCombinators

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

  val ( --> ) :
    ('a, 'b, 'c, 'd, 'e, 'f inp) role ->
    ('g, 'e, 'h, 'c, 'b, 'i) role ->
    ('i, 'a out, 'f, 'g) label ->
    'h global ->
    'd global
end

module Make (X : C) : S with type chan := X.chan
include S with type chan = DynChan.chan
