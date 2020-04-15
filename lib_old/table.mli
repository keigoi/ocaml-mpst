type 'k t
val create : unit -> 'a t
val create_from : 'a list -> 'a t
val put : 'a t -> int -> 'a -> unit
val get : 'a t -> int -> 'a
val get_or_add : 'a t -> int -> (unit -> 'a) -> 'a
val get_opt : 'a t -> int -> 'a option
val size : 'a t -> int
val to_list : 'a t -> 'a list
