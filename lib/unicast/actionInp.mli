type 'var branch
type ('v, 's) inp

val make_branch :
  ('a, 'c branch) Rows.method_ ->
  ('c, 'd) Rows.constr ->
  int DynChan.name ->
  'd LinState.t ->
  'a LinState.t

val make_inp :
  ('a, ('v, 'c) inp) Rows.method_ ->
  'v DynChan.name ->
  'c LinState.t ->
  'a LinState.t

(* val branch_state :
  ('a, 'c branch) Rows.method_ ->
  ('c, 'd) Rows.constr ->
  int DynChan.name ->
  'd LinState.t ->
  'a Lin.gen *)

(* val branch_ops :
  ('a, 'c branch) Rows.method_ ->
  (module State.StateOp with type a = 'a Lin.gen) *)

val branch : 'var branch -> 'var
val receive : ('v, 's) inp -> 'v * 's
