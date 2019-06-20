type 'k t
val create : (int -> 'a) -> 'a t
val create_with : (int -> 'a) -> 'a list -> 'a t
val extend : 'a t -> int -> unit
val put : 'a t -> ('b, 'c, 'd, 'e) Seq.lens -> 'a -> unit
val get : 'a t -> ('b, 'c, 'd, 'e) Seq.lens -> 'a
val get_or_create :
  'a t -> ('b, 'c, 'd, 'e) Seq.lens -> int -> 'a
val get_or_create_ : 'a t -> int -> int -> 'a
val size : 'a t -> int
