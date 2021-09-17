type 'var wrapped_state

val make_wrapped :
  ('a, 'b) Types.constr -> unit Name.t -> 'b State.t -> 'a wrapped_state

val merge_wrapped_states :
  'a wrapped_state -> 'a wrapped_state -> 'a wrapped_state

val determinise_wrapped :
  dict:StateHash.dict -> 'a wrapped_state -> unit

val make_event_from_determinised_ :
  'a wrapped_state -> 'a Event.event
