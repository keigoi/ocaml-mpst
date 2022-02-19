type 'a t
type 'a mergefun = 'a -> 'a -> 'a
type 'a mergenextfun = StateHash.dict -> 'a -> unit
type cache = StateHash.dict

exception UnguardedLoop of string

val make : 'a mergefun -> 'a mergenextfun -> 'a -> 'a t
val make_internal_choice : 'l t -> 'r t -> ('lr, 'l, 'r) Types.disj -> 'lr t
val make_unbound : unit -> 'a t
val bind_state : from:'a t -> to_:'a t -> unit
val unit : unit t
val merge : 'a t -> 'a t -> 'a t
val determinise : cache:cache -> 'a t -> 'a
val determinised_ : 'a t -> 'a

type 'a head = 'a StateHash.head
(** for internal use in wrapped.ml *)

type 'a id = 'a StateHash.state_id
(** for internal use in wrapped.ml *)

val epsilon_closure : 'a t -> 'a id * 'a head list
(** for internal use in wrapped.ml *)

val determinise_heads : cache:cache -> 'a id -> 'a head list -> 'a head
(** for internal use in wrapped.ml *)
