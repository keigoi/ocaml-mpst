type 'var branch

val make_branch :
  ('a, 'c branch) Rows.method_ ->
  ('c, 'd) Rows.constr ->
  int DynChan.name ->
  'd LinState.t ->
  'a LinState.t

val branch_state :
  ('a, 'c branch) Rows.method_ ->
  ('c, 'd) Rows.constr ->
  int DynChan.name ->
  'd LinState.t ->
  'a Lin.gen

val branch_ops :
  ('a, 'c branch) Rows.method_ -> (module State.DetState with type a = 'a Lin.gen)

val branch : 'var branch -> 'var
