type 'a mergefun = 'a -> 'a -> 'a
type 'a mergenextfun = StateHash.dict -> 'a -> unit

type 'a state

exception UnguardedLoop of string

val make_state :
  'a mergefun -> 'a mergenextfun -> 'a -> 'a state

val make_internal_choice :
  'l state ->
  'r state ->
  ('lr, 'l, 'r) Types.disj ->
  'lr state

val merge_state : 'a state -> 'a state -> 'a state

val make_unbound_state : unit -> 'a state

val bind_state : from:'a state -> to_:'a state -> unit

val determinise :
  dict:StateHash.dict -> 'a state -> 'a

val determinised_ :
  'a state -> 'a

type 'var wrapped_state

val make_wrapped :
  ('a, 'b) Types.constr -> unit Names.name -> 'b state -> 'a wrapped_state

val merge_wrapped_states :
  'a wrapped_state -> 'a wrapped_state -> 'a wrapped_state

val determinise_wrapped :
  dict:StateHash.dict -> 'a wrapped_state -> unit

val make_event_from_determinised_ :
  'a wrapped_state -> 'a Event.event
