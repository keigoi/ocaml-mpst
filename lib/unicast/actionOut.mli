module Make (C : S.CHANNEL) : sig
  type 'a select
  type ('v, 'a) out

  val make_select :
    ('a, 'b) Rows.method_ ->
    ('b, 'c select) Rows.method_ ->
    int C.endpoint ->
    'c LinState.t ->
    'a LinState.t

  val select : 's select -> 's

  val make_out :
    ('a, ('v, 'c) out) Rows.method_ ->
    'v C.endpoint ->
    'c LinState.t ->
    'a LinState.t

  val send : ('v, 's) out -> 'v -> 's
end
