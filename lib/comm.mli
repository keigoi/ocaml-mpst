open BasicCombinators

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
