type 'a many

module Make (C : S.CHANNEL) : sig
  val get_many : 'a many -> 'a list
  val units : count:int -> unit many LinState.t

  val make_selects :
    count:int ->
    ('a, 'b) Rows.method_ ->
    ('b, 'c ActionOut.Make(C).select) Rows.method_ ->
    int C.endpoint list ->
    'c many LinState.t ->
    'a many LinState.t

  val make_branches :
    count:int ->
    ('a, 'b ActionInp.Make(C).branch) Rows.method_ ->
    ('b, 'c) Rows.constr ->
    int C.endpoint list ->
    'c many LinState.t ->
    'a many LinState.t

  val make_outs :
    count:int ->
    ('a, ('v, 'c) ActionOut.Make(C).out) Rows.method_ ->
    'v C.endpoint list ->
    'c many LinState.t ->
    'a many LinState.t

  val make_inps :
    count:int ->
    ('a, ('v, 'c) ActionInp.Make(C).inp) Rows.method_ ->
    'v C.endpoint list ->
    'c many LinState.t ->
    'a many LinState.t
end
