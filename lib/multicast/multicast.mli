open BasicCombinators

module Make (C : S.CHANNEL) : sig
  type 's scatter
  type 's gather
  type ('v, 's) scatter_val
  type ('v, 's) gather_val
  type 's many

  val ( -->@@ ) :
    ('a, 'b, 'c, 'd, 'e, 'f Unicast.Make(C).branch) BasicCombinators.role ->
    ('g many, 'e many, 'h, 'c, 'b, 'i) BasicCombinators.role ->
    ('i, 'a scatter, 'f, 'g) BasicCombinators.label ->
    'h global ->
    'd global

  val ( @@--> ) :
    ('a many, 'b many, 'c, 'd, 'e, 'f gather) BasicCombinators.role ->
    ('g, 'e, 'h, 'c, 'b, 'i) BasicCombinators.role ->
    ('i, 'a Unicast.Make(C).select, 'f, 'g) BasicCombinators.label ->
    'h global ->
    'd global

  val ( ==>@@ ) :
    ('a, 'b, 'c, 'd, 'e, ('f, 'g) Unicast.Make(C).inp) role ->
    ('g many, 'e many, 'h, 'c, 'b, ('f, 'a) ActionScatter.Make(C).scatter_val) role ->
    'h global ->
    'd global

  val ( @@==> ) :
    ('a many, 'b many, 'c, 'd, 'e, ('f, 'g) ActionGather.Make(C).gather_val) role ->
    ('g, 'e, 'h, 'c, 'b, ('f, 'a) Unicast.Make(C).out) role ->
    'h global ->
    'd global

  val many_at :
    (unit, unit many, 'b, 'c, 'd, 'e) BasicCombinators.role ->
    'b global ->
    'c global

  type param += P : ((_, _, _, _, _, _) role * int) -> param

  val get_many : 'a many -> 'a list
  val scatter : 'a scatter -> 'a
  val gather : 'a gather -> 'a
  val scatter_val : ('v, 'a) scatter_val -> 'v list -> 'a
  val gather_val : ('v, 'a) gather_val -> 'v list * 'a
end
