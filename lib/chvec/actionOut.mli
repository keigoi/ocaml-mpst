type 'a out

val out :
  ('a, 'b) Rows.method_ ->
  ('b, 'c out) Rows.method_ ->
  int DynChan.name ->
  'c State.t ->
  'a State.t

val out_state :
  ('a, 'b) Rows.method_ ->
  ('b, 'c out) Rows.method_ ->
  int DynChan.name ->
  'c State.t ->
  'a

val out_ops :
  ('a, 'b) Rows.method_ ->
  ('b, 'c out) Rows.method_ ->
  (module State.DetState with type a = 'a)

val select : 's out -> 's
