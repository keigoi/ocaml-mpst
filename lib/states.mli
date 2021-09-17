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

val determinise_heads : dict:StateHash.dict -> 'a StateHash.state_id -> 'a StateHash.head list -> 'a StateHash.head

val epsilon_closure : 'a state -> 'a StateHash.state_id * 'a StateHash.head list
