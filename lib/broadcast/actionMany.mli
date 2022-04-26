type 'a many = Many of 'a State.t list

val get_many : 'a many -> 'a list

val outs :
  count:int ->
  ('a, 'b) Rows.method_ ->
  ('b, 'c ActionOut.out) Rows.method_ ->
  int DynChan.name list ->
  'c many State.t ->
  'a many State.t

val inps :
  count:int ->
  ('a, 'b ActionInp.inp) Rows.method_ ->
  ('b, 'c) Rows.constr ->
  int DynChan.name list ->
  'c many State.t ->
  'a many State.t
