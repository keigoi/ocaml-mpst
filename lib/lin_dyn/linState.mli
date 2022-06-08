type 'a t = 'a Lin.gen PowState.t

val gen_ops :
  (module State.StateOp with type a = 'b) ->
  (module State.StateOp with type a = 'b Lin.gen)

val make_lin_state : 'a State.t -> 'a Lin.lin Lin.gen State.t
val make_unlimited_state : 'a State.t -> 'a Lin.gen State.t

val map :
  ('a -> 'b) ->
  ('b -> 'a) ->
  (string -> string) ->
  'a Lin.gen State.t ->
  'b Lin.gen State.t

val map_method :
  ('a, 'b) Rows.method_ -> 'b Lin.gen State.t -> 'a Lin.gen State.t

val unit : unit Lin.gen PowState.t
