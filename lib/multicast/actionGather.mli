type 'var gather

val make_gather :
  ('a, 'var gather) Rows.method_ ->
  ('var, 's) Rows.constr ->
  int DynChan.name list ->
  's LinState.t ->
  'a LinState.t

val gather : 'a gather -> 'a
