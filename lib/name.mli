include S.LOCAL

val create : ('a -> 'b) -> 'a out * 'b inp
val create_scatter : int -> (int -> 'a -> 'b) -> 'a scatter * 'b inp list
val create_gather : int -> ('a list -> 'b) -> 'a out list * 'b gather

