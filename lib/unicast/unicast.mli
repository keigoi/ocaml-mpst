open BasicCombinators

module Make (C : S.CHANNEL) : sig
  type 's select = 's ActionOut.Make(C).select
  type 's branch = 's ActionInp.Make(C).branch
  type ('v, 's) out = ('v, 's) ActionOut.Make(C).out
  type ('v, 's) inp = ('v, 's) ActionInp.Make(C).inp

  val select : 'a select -> 'a
  val branch : 'a branch -> 'a
  val send : ('v, 'a) out -> 'v -> 'a
  val receive : ('v, 'a) inp -> 'v * 'a
  val close : unit -> unit

  val ( --> ) :
    ('a, 'b, 'c, 'd, 'e, 'f branch) role ->
    ('g, 'e, 'h, 'c, 'b, 'i) role ->
    ('i, 'a select, 'f, 'g) label ->
    'h global ->
    'd global

  val ( ==> ) :
    ('a, 'b, 'c, 'd, 'e, ('f, 'g) inp) role ->
    ('g, 'e, 'h, 'c, 'b, ('f, 'a) out) role ->
    'h global ->
    'd global
end
