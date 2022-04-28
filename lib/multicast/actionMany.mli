type 'a many

val get_many : 'a many -> 'a list
val units : count:int -> unit many LinState.t

val outs :
  count:int ->
  ('a, 'b) Rows.method_ ->
  ('b, 'c ActionOut.out) Rows.method_ ->
  int DynChan.name list ->
  'c many LinState.t ->
  'a many LinState.t

val inps :
  count:int ->
  ('a, 'b ActionInp.inp) Rows.method_ ->
  ('b, 'c) Rows.constr ->
  int DynChan.name list ->
  'c many LinState.t ->
  'a many LinState.t
