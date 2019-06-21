type 'k t
val create : (int -> 'a) -> 'a t
val create_with : (int -> 'a) -> 'a list -> 'a t
val extend : 'a t -> int -> unit
val put : 'a t -> int -> 'a -> unit
val get : 'a t -> int -> 'a
val get_or_create :
  'a t -> int -> int -> 'a
val get_or_create_ : 'a t -> int -> int -> 'a
val size : 'a t -> int
