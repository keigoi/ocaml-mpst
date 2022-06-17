type 'var gather
type ('v, 's) gather_val

val make_gather :
  ('a, 'var gather) Rows.method_ ->
  ('var, 's) Rows.constr ->
  int DynChan.endpoint list ->
  's LinState.t ->
  'a LinState.t

val make_gather_val :
  ('a, ('v, 's) gather_val) Rows.method_ ->
  'v DynChan.endpoint list ->
  's LinState.t ->
  'a LinState.t

val gather : 'a gather -> 'a
val gather_val : ('v, 's) gather_val -> 'v list * 's
