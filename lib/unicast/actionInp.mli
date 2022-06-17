module Make (C : S.CHANNEL) : sig
  type 'var branch
  type ('v, 's) inp

  val make_branch :
    ('a, 'c branch) Rows.method_ ->
    ('c, 'd) Rows.constr ->
    int C.endpoint ->
    'd LinState.t ->
    'a LinState.t

  val make_inp :
    ('a, ('v, 'c) inp) Rows.method_ ->
    'v C.endpoint ->
    'c LinState.t ->
    'a LinState.t

  val branch : 'var branch -> 'var
  val receive : ('v, 's) inp -> 'v * 's
end
