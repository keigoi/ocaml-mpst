type 'a many

val get_many : 'a many -> 'a list
val units : count:int -> unit many LinState.t

val make_selects :
  count:int ->
  ('a, 'b) Rows.method_ ->
  ('b, 'c ActionOut.select) Rows.method_ ->
  int DynChan.endpoint list ->
  'c many LinState.t ->
  'a many LinState.t

val make_branches :
  count:int ->
  ('a, 'b ActionInp.branch) Rows.method_ ->
  ('b, 'c) Rows.constr ->
  int DynChan.endpoint list ->
  'c many LinState.t ->
  'a many LinState.t

val make_outs :
  count:int ->
  ('a, ('v, 'c) ActionOut.out) Rows.method_ ->
  'v DynChan.endpoint list ->
  'c many LinState.t ->
  'a many LinState.t

val make_inps :
  count:int ->
  ('a, ('v, 'c) ActionInp.inp) Rows.method_ ->
  'v DynChan.endpoint list ->
  'c many LinState.t ->
  'a many LinState.t
