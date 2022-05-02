type 'a select
type ('v, 'a) out

val make_select :
  ('a, 'b) Rows.method_ ->
  ('b, 'c select) Rows.method_ ->
  int DynChan.name ->
  'c LinState.t ->
  'a LinState.t

val select_state :
  ('a, 'b) Rows.method_ ->
  ('b, 'c select) Rows.method_ ->
  int DynChan.name ->
  'c LinState.t ->
  'a Lin.gen

val select_ops :
  ('a, 'b) Rows.method_ ->
  ('b, 'c select) Rows.method_ ->
  (module State.DetState with type a = 'a Lin.gen)

val select : 's select -> 's

val make_out :
  ('a, ('v, 'c) out) Rows.method_ ->
  'v DynChan.name ->
  'c LinState.t ->
  'a LinState.t

val send : ('v, 's) out -> 'v -> 's
