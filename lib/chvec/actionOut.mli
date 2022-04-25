type 'a out

val out :
  ('a, 'b) Rows.method_ ->
  ('b, 'c out) Rows.method_ ->
  int DynChan.name ->
  'c State.t ->
  'a State.t

val select : 's out -> 's
