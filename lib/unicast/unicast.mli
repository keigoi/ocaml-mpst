open BasicCombinators

type 's select = 's ActionOut.select
type 's branch = 's ActionInp.branch
type ('v, 's) out = ('v, 's) ActionOut.out
type ('v, 's) inp = ('v, 's) ActionInp.inp

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
