type 'var gather

val make_gather :
  ('a, 'var gather) Rows.method_ ->
  ('var, 's) Rows.constr ->
  int DynChan.name list ->
  's State.t ->
  'a State.t

val gather : 'a gather -> 'a
