open BasicCombinators

type 's select = 's ActionOut.select
type 's branch = 's ActionInp.branch
type chan

val select : 'a select -> 'a
val branch : 'a branch -> 'a
val close : unit -> unit

val ( --> ) :
  ('a, 'b, 'c, 'd, 'e, 'f branch) role ->
  ('g, 'e, 'h, 'c, 'b, 'i) role ->
  ('i, 'a select, 'f, 'g) label ->
  'h global ->
  'd global
