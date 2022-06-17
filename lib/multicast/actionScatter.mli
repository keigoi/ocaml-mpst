type _ scatter
type (_, _) scatter_val

val make_scatter :
  ('a, 'b) Rows.method_ ->
  ('b, 'c scatter) Rows.method_ ->
  int DynChan.endpoint list ->
  'c LinState.t ->
  'a LinState.t

val make_scatter_val :
  ('a, ('v, 'c) scatter_val) Rows.method_ ->
  'v DynChan.endpoint list ->
  'c LinState.t ->
  'a LinState.t

val scatter : 'a scatter -> 'a
val scatter_val : ('v, 'a) scatter_val -> 'v list -> 'a
