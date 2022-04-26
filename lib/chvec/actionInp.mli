type 'var inp

val inp :
  ('a, 'c inp) Rows.method_ ->
  ('c, 'd) Rows.constr ->
  int DynChan.name ->
  'd State.t ->
  'a State.t

val inp_state :
  ('a, 'c inp) Rows.method_ ->
  ('c, 'd) Rows.constr ->
  int DynChan.name ->
  'd State.t ->
  'a

val inp_ops :
  ('a, 'c inp) Rows.method_ ->
  (module State.DetState with type a = 'a)

val branch : 'var inp -> 'var
