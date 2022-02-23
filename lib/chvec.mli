type _ out
type 'var inp

val select : 'a out -> 'a
val branch : 'a inp -> 'a

val out :
  ('a, 'b) Rows.method_ ->
  ('b, 'c out) Rows.method_ ->
  unit Name.t ->
  'c State.t ->
  'a State.t

val inp :
  ('a, 'b inp) Rows.method_ ->
  ('b, 'c) Rows.constr ->
  unit Name.t ->
  'c State.t ->
  'a State.t
