open BasicCombinators

type 's scatter
type 's gather
type 's many

val ( -->@@ ) :
  ('a, 'b, 'c, 'd, 'e, 'f Unicast.branch) BasicCombinators.role ->
  ('g many, 'e many, 'h, 'c, 'b, 'i) BasicCombinators.role ->
  ('i, 'a scatter, 'f, 'g) BasicCombinators.label ->
  'h global ->
  'd global

val ( @@--> ) :
  ('a many, 'b many, 'c, 'd, 'e, 'f gather) BasicCombinators.role ->
  ('g, 'e, 'h, 'c, 'b, 'i) BasicCombinators.role ->
  ('i, 'a Unicast.select, 'f, 'g) BasicCombinators.label ->
  'h global ->
  'd global

val many_at :
  (unit, unit many, 'b, 'c, 'd, 'e) BasicCombinators.role ->
  'b global ->
  'c global

type param += P : ((_, _, _, _, _, _) role * int) -> param

val get_many : 'a many -> 'a list
val scatter : 'a scatter -> 'a
val gather : 'a gather -> 'a
