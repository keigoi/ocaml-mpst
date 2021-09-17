type 'a mergefun = 'a -> 'a -> 'a
type 'a mergenextfun = StateHash.dict -> 'a -> unit

type 'a t

exception UnguardedLoop of string

val make_state :
  'a mergefun -> 'a mergenextfun -> 'a -> 'a t

val make_internal_choice :
  'l t ->
  'r t ->
  ('lr, 'l, 'r) Types.disj ->
  'lr t

val merge_state : 'a t -> 'a t -> 'a t

val make_unbound_state : unit -> 'a t

val bind_state : from:'a t -> to_:'a t -> unit

val determinise :
  dict:StateHash.dict -> 'a t -> 'a

val determinised_ :
  'a t -> 'a

(** for internal use in wrapped.ml *)
val epsilon_closure : 'a t -> 'a StateHash.state_id * 'a StateHash.head list

(** for internal use in wrapped.ml *)
val determinise_heads : dict:StateHash.dict -> 'a StateHash.state_id -> 'a StateHash.head list -> 'a StateHash.head
