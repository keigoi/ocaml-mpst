type 'a out

val make_out :
  ('a, 'b) Rows.method_ ->
  ('b, 'c out) Rows.method_ ->
  int DynChan.name ->
  'c LinState.t ->
  'a LinState.t

val out_state :
  ('a, 'b) Rows.method_ ->
  ('b, 'c out) Rows.method_ ->
  int DynChan.name ->
  'c LinState.t ->
  'a Lin.gen

val out_ops :
  ('a, 'b) Rows.method_ ->
  ('b, 'c out) Rows.method_ ->
  (module State.DetState with type a = 'a Lin.gen)

val select : 's out -> 's
