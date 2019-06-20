type 'k t
val create : (unit -> 'a) -> 'a t
val extend : 'a t -> int -> unit
val put : 'a t -> ('b, 'c, 'd, 'e) Seq.lens -> 'a list -> unit
val get : 'a t -> ('b, 'c, 'd, 'e) Seq.lens -> 'a list
val get_or_create :
  'a t -> ('b, 'c, 'd, 'e) Seq.lens -> int -> 'a list
