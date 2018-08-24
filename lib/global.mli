open Base
open Session

type 'a mpst = MPST of 'a list lazy_t
type ('a, 'b, 'c, 'd, 'r) role = ('a, 'b, 'c, 'd) lens * 'r

type ('d1, 'd, 'f1, 'f) commm
type ('l, 'x1, 'x2, 'r, 'y1, 'y2) commm2

val get_sess : ('s sess, 'b, 'a mpst, 'c, 'd) role -> 'a mpst -> 's sess
val uget : ('s sess, _, 'a, _) lens -> 'a mpst -> 's sess

exception RoleNotEnabled

val ( --> ) :
  ('d sess, ('r2 * 'd1) select sess, 's mpst, 't mpst, 'r1) role ->
  ('f sess, ('r1 * 'f1) branch sess, 't mpst, 'u mpst, 'r2) role ->
  ('d1, 'd sess, 'f1, 'f sess) commm ->
  's mpst ->
  'u mpst

val ( -%%-> ) :
  (close sess, ('r2 * 'l) select sess, 'ss mpst, 't mpst, 'r1) role ->
  (close sess, ('r1 * 'r) branch sess, 't mpst, 'u mpst, 'r2) role ->
  ('l, 'd1 sess, 'd2 sess, 'r, 'f1 sess, 'f2 sess) commm2 ->
  l1:(('d1 sess, close sess, 's1 mpst, 't1 mpst, 'r1) role *
        ('f1 sess, close sess, 't1 mpst, 'ss mpst, 'r2) role) *
    's1 mpst ->
  l2:(('d2 sess, close sess, 's2 mpst, 't2 mpst, 'r1) role *
        ('f2 sess, close sess, 't2 mpst, 'ss mpst, 'r2) role) *
    's2 mpst ->
  'u mpst

val ( -!-> ) :
  ('d sess, ('r2 * ('k1 -> 'd1)) request sess, 's mpst, 't mpst, 'r1) role ->
  ('f sess, ('r1 * ('k2 -> 'f1)) accept sess, 't mpst, 'u mpst, 'r2) role ->
  (('r1, 'k1) conn * ('r2, 'k2) conn -> ('d1, 'd sess, 'f1, 'f sess) commm) ->
  (('r1, 'k1) conn * ('r2, 'k2) conn -> 's mpst) ->
  'u mpst

val ( -?-> ) :
  ('d sess, ('r2 * 'd1) select sess, 's mpst, 't mpst, 'r1) role ->
  ('f sess, ('r1 * 'f1) branch sess, 't mpst, 'u mpst, 'r2) role ->
  ('d1, ('r2 * ('r1, 'k1) conn * 'd sess) disconnect sess, 'f1,
   ('r1 * ('r2, 'k2) conn * 'f sess) disconnect sess) commm ->
  ('r2, 'k2) conn * ('r1, 'k1) conn ->
  's mpst ->
  'u mpst

val (-!%%->) : (close sess, ('r2 * ('k1 -> 'l)) request sess, 'ss mpst, 't mpst, 'r1) role ->
               (close sess, ('r1 * ('k2 -> 'r)) accept sess, 't mpst, 'u mpst, 'r2) role ->
               (('r1, 'k1) conn * ('r2, 'k2) conn -> ('l, 'd1 sess, 'd2 sess, 'r, 'f1 sess, 'f2 sess) commm2) ->
               l1:(('d1 sess, close sess, 's1 mpst, 't1 mpst, 'r1) role *
                     ('f1 sess, close sess, 't1 mpst, 'ss mpst, 'r2) role) *
                 (('r1, 'k1) conn * ('r2, 'k2) conn -> 's1 mpst) ->
               l2:(('d2 sess, close sess, 's2 mpst, 't2 mpst, 'r1) role *
                     ('f2 sess, close sess, 't2 mpst, 'ss mpst, 'r2) role) *
                 (('r1, 'k1) conn * ('r2, 'k2) conn -> 's2 mpst) ->
               'u mpst

val (-?%%->) : (close sess, ('r2 * 'l) select sess, 'ss mpst, 't mpst, 'r1) role ->
               (close sess, ('r1 * 'r) branch sess, 't mpst, 'u mpst, 'r2) role ->
               ('l, ('r2 * ('r1, 'k1) conn * 'd1 sess) disconnect sess,
                ('r2 * ('r1, 'k1) conn * 'd2 sess) disconnect sess, 'r,
                ('r1 * ('r2, 'k2) conn * 'f1 sess) disconnect sess,
                ('r1 * ('r2, 'k2) conn * 'f2 sess) disconnect sess)
                 commm2 ->
               ('r2, 'k2) conn * ('r1, 'k1) conn ->
               l1:(('d1 sess, close sess, 's1 mpst, 't1 mpst, 'r1) role *
                     ('f1 sess, close sess, 't1 mpst, 'ss mpst, 'r2) role) *
                 's1 mpst ->
               l2:(('d2 sess, close sess, 's2 mpst, 't2 mpst, 'r1) role *
                     ('f2 sess, close sess, 't2 mpst, 'ss mpst, 'r2) role) *
                 's2 mpst ->
               'u mpst
val discon :
  ('d sess, ('r2 * ('r1, 'k1) conn * 'd sess) disconnect sess, 's mpst, 't mpst, 'r1)
    role ->
  ('f sess, ('r1 * ('r2, 'k2) conn * 'f sess) disconnect sess, 't mpst, 'u mpst, 'r2)
    role ->
  ('r1, 'k1) conn * ('r2, 'k2) conn ->
  's mpst ->
  'u mpst

val dummy_close :
  (close sess, close sess, 'a mpst, 'a mpst, 'r) role -> 'a mpst -> 'a mpst
val dummy_receive :
  ('l branch sess, 'l branch sess, 'a mpst, 'a mpst, 'r1) role ->
  'a mpst -> 'a mpst

val loop_ : 'a mpst lazy_t -> 'a mpst

module Labels : sig
  val mklabel :
    (('a -> 'b) -> 'c) ->
    ('d * 'e -> 'f) ->
    ('g -> 'a -> unit) ->
    ('h -> 'd Lwt.t) -> 'g * 'h -> ('c, 'b, 'f, 'e) commm

  val mklabel2 :
    (('a -> 'b) -> ('c -> 'd) -> 'e) ->
    ('f * 'g -> 'h) ->
    ('i * 'j -> 'h) ->
    ('k -> ('a, 'c) Base.either -> 'l) ->
    ('m -> ('f, 'i) Base.either Lwt.t) ->
    'k * 'm -> ('e, 'b, 'd, 'h, 'g, 'j) commm2
end
