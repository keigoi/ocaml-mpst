type _ t
and _ out
and 'var inp

exception UnguardedLoop of string

val unit : unit t

val out :
  ('a, 'b) Types.method_ ->
  ('b, 'c out) Types.method_ ->
  unit Name.t ->
  'c t ->
  'a t

val inp :
  ('a, 'b inp) Types.method_ ->
  ('b, 'c) Types.constr ->
  unit Name.t ->
  'c t ->
  'a t

val merge : 'a t -> 'a t -> 'a t
val internal_choice : ('a, 'b, 'c) Types.disj -> 'b t -> 'c t -> 'a t
val loop : 'a t lazy_t -> 'a t
val determinise : 'a t -> 'a
val select : 's out -> 's
val branch : 'var inp -> 'var
