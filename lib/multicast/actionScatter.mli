type _ scatter

val make_scatter :
  ('a, 'b) Rows.method_ ->
  ('b, 'c scatter) Rows.method_ ->
  int DynChan.name list ->
  'c LinState.t ->
  'a LinState.t

val scatter : 'a scatter -> 'a
