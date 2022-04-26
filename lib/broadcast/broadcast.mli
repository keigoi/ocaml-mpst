open BasicCombinators

type 's scatter
type 's gather
type 's many

type chan_table = {
  process_count : (string, int) Hashtbl.t;
  table : (string * string, DynChan.chan list) Hashtbl.t;
}

type BasicCombinators.env_entry += Broadcast of chan_table

val ( --<< ) :
  ('a, 'b, 'c, 'd, 'e, 'f Comm.inp) BasicCombinators.role ->
  ('g many, 'e many, 'h, 'c, 'b, 'i) BasicCombinators.role ->
  ('i, 'a scatter, 'f, 'g) BasicCombinators.label ->
  'h global ->
  'd global

val ( -->> ) :
  ('a many, 'b many, 'c, 'd, 'e, 'f gather) BasicCombinators.role ->
  ('g, 'e, 'h, 'c, 'b, 'i) BasicCombinators.role ->
  ('i, 'a Comm.out, 'f, 'g) BasicCombinators.label ->
  'h global ->
  'd global

val many_at :
  (unit, unit many, 'b, 'c, 'd, 'e) BasicCombinators.role ->
  'b global ->
  'c global

val get_many : 'a many -> 'a list
val scatter : 'a scatter -> 'a
val gather : 'a gather -> 'a
