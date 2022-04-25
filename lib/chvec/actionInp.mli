type 'var inp

val inp :
  ('a, 'c inp) Rows.method_ ->
  ('c, 'd) Rows.constr ->
  int DynChan.name ->
  'd State.t ->
  'a State.t

val branch : 'var inp -> 'var
