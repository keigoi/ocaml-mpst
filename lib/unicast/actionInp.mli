type 'var branch
type ('v, 's) inp

val make_branch :
  ('a, 'c branch) Rows.method_ ->
  ('c, 'd) Rows.constr ->
  int DynChan.endpoint ->
  'd LinState.t ->
  'a LinState.t

val make_inp :
  ('a, ('v, 'c) inp) Rows.method_ ->
  'v DynChan.endpoint ->
  'c LinState.t ->
  'a LinState.t

val branch : 'var branch -> 'var
val receive : ('v, 's) inp -> 'v * 's
