open Base

module type LOCAL = sig
  type 'v cell = ('v Lwt.t * 'v Lwt.u) ref
  type _ sess =
    | Send : ('r * 'v cell * 's sess) -> [`send of 'r * [`msg of 'v * 's]] sess
    | Recv : ('r * 'v cell * 's sess) -> [`recv of 'r * [`msg of 'v * 's]] sess
    | SelectLeft : 'r * bool cell * 's sess -> [`send of 'r * [`left of 's]] sess
    | SelectRight : 'r * bool cell * 's sess -> [`send of 'r * [`right of 's]] sess
    | BranchLeft : 'r * bool cell * 's1 sess -> [`recv of 'r * [`left of 's1 | `right of 's2]] sess
    | BranchRight : 'r * bool cell * 's2 sess -> [`recv of 'r * [`left of 's1 | `right of 's2]] sess
    | SelectLeftRight : 'r * bool cell * 's1 sess * 's2 sess -> [`send of 'r * [`left of 's1 | `right of 's2]] sess
    | BranchLeftRight : 'r * bool cell * 's1 sess * 's2 sess -> [`recv of 'r * [`left of 's1 | `right of 's2]] sess
    | Close : [`close] sess

  val send :
    'r -> 'v -> [ `send of 'r * [ `msg of 'v * 's ] ] sess -> 's sess
  val receive :
    'r -> [ `recv of 'r * [ `msg of 'v * 's ] ] sess -> ('v * 's sess) Lwt.t
  val select_left : 'r -> [ `send of 'r * [ `left of 's ] ] sess -> 's sess
  val select_left_ :
    'r -> [ `send of 'r * [ `left of 's | `right of 'a ] ] sess -> 's sess
  val select_right : 'r -> [ `send of 'r * [ `right of 's ] ] sess -> 's sess
  val select_right_ :
    'r -> [ `send of 'r * [ `left of 'a | `right of 's ] ] sess -> 's sess
  val branch :
    'r ->
    [ `recv of 'r * [ `left of 's1 | `right of 's2 ] ] sess ->
    ('s1 sess, 's2 sess) either Lwt.t
  val close : [ `close ] sess -> unit
end

module type GLOBAL = sig
  type 'a mpst
  type 'a sess
  type ('v1, 'v2, 's1, 's2) lens = {
    get : 's1 -> 'v1;
    put : 's1 -> 'v2 -> 's2;
  }
  type ('l, 'x) label = Label of ('x -> 'l)
  type ('a, 'b, 'c, 'd, 'r) role = ('a, 'b, 'c, 'd) lens * 'r
  type _ typ = ..
  type _ typ += Int : int typ | String : string typ
  type (_, _, _, _) dlabel =
      DMsg :
        't typ -> ([ `msg of 't * 'g ], 'g, [ `msg of 't * 'h ], 'h) dlabel
    | DLeft :
        ([ `left of 'g ], 'g, [ `left of 'h | `right of 'a ], 'h) dlabel
    | DRight :
        ([ `right of 'g ], 'g, [ `left of 'b | `right of 'h ], 'h) dlabel
  val unify : 's sess -> 's sess -> 's sess
  val get_sess : ('s sess, 'b, 'a, 'c, 'd) role -> 'a mpst -> 's sess
  val put_sess : ('c, 't, 'a, 'b, 'd) role -> 'a mpst -> 't -> 'b mpst
  val ( --> ) :
    ('d sess, [ `send of 'r2 * 'd1 ] sess, 's, 't, 'r1) role ->
    ('f sess, [ `recv of 'r1 * 'f1 ] sess, 't, 'u, 'r2) role ->
    ('d1, 'd, 'f1, 'f) dlabel -> 's mpst -> 'u mpst
  val ( -%%-> ) :
    (unit, [ `send of 'r2 * [ `left of 'd1 | `right of 'd2 ] ] sess, 'ss, 't,
     'r1)
    role ->
    (unit, [ `recv of 'r1 * [ `left of 'f1 | `right of 'f2 ] ] sess, 't, 'u,
     'r2)
    role ->
    left:(('d1 sess, unit, 's1, 't1, 'r1) role *
          ('f1 sess, unit, 't1, 'ss, 'r2) role) *
         's1 mpst ->
    right:(('d2 sess, unit, 's2, 't2, 'r1) role *
           ('f2 sess, unit, 't2, 'ss, 'r2) role) *
          's2 mpst ->
    'u mpst

  type a = A
  type b = B
  type c = C

  val a :
    ('a1, 'a2, < a : 'a1; b : 'b; c : 'c >, < a : 'a2; b : 'b; c : 'c >, a)
    role
  val b :
    ('b1, 'b2, < a : 'a; b : 'b1; c : 'c >, < a : 'a; b : 'b2; c : 'c >, b)
    role
  val c :
    ('c1, 'c2, < a : 'a; b : 'b; c : 'c1 >, < a : 'a; b : 'b; c : 'c2 >, c)
    role
  val finish :
    < a : [ `close ] sess; b : [ `close ] sess; c : [ `close ] sess > mpst
  val msg :
    'a typ -> ([ `msg of 'a * 'b ], 'b, [ `msg of 'a * 'c ], 'c) dlabel
  val left : ([ `left of 'a ], 'a, [ `left of 'b | `right of 'c ], 'b) dlabel
  val right :
    ([ `right of 'a ], 'a, [ `left of 'b | `right of 'c ], 'c) dlabel
  val int : int typ
  val str : string typ
end
