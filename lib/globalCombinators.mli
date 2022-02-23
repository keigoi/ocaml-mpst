type all_unit = [ `cons of unit * 'a ] as 'a

module Open : sig
  type _ t =
    | [] : all_unit t
    | ( :: ) : (unit, 'b, 'bb, 'cc, 'a, 'c) Types.role * 'bb t -> 'cc t
end

type 's out
type 's inp

type 'a seq = 'a Hlist.Make(State).seq =
  | ( :: ) : 'hd State.t * 'tl seq -> [ `cons of 'hd * 'tl ] seq
  | [] : ([ `cons of unit * 'a ] as 'a) seq

exception UnguardedLoop of string

val select : 'a out -> 'a
val branch : 'a inp -> 'a
val close : unit -> unit

val ( --> ) :
  ('a, 'b, 'c, 'd, 'e, 'f inp) Types.role ->
  ('g, 'e, 'h, 'c, 'b, 'i) Types.role ->
  ('i, 'a out, 'f, 'g) Types.label ->
  'h seq ->
  'd seq

val choice_at :
  ('a, 'b, 'c, 'd, 'e, 'f) Types.role ->
  ('b, 'g, 'h) Rows.disj ->
  ('g, unit, 'i, 'c, 'j, 'k) Types.role * 'i seq ->
  ('h, unit, 'l, 'c, 'm, 'n) Types.role * 'l seq ->
  'd seq

val fix_with : 'a Open.t -> ('a seq -> 'a seq) -> 'a seq
val finish : ([ `cons of unit * 'a ] as 'a) seq
val extract : 'u seq -> 'u
