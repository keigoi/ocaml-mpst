open Base
open Concur_shims

type ('v, 's) out
type 'var inp
type close
type ('v, 's) scatter
type 'var gather

val send : ('v, 't) out -> 'v -> 't IO.io
val receive : 'var inp -> 'var IO.io
val close : close -> unit IO.io
val send_list : ('v, 't) scatter -> (int -> 'v) -> 't IO.io
val receive_list : 'var gather -> 'var IO.io

type 't global
type 't tup

val ( --> ) :
  ('a one, 'b one, 'c, 'd, 'e, 'f inp) role ->
  ('g one, 'e one, 'h, 'c, 'b, 'i) role ->
  ('i, ('j, 'a) out, 'f, 'j * 'g) label -> 'h global -> 'd global

val gather :
  ('a list, 'b list, 'c, 'd, 'e, 'f gather) role ->
  ('g one, 'e one, 'h, 'c, 'b, 'i) role ->
  ('i, ('j, 'a) out, 'f, 'j list * 'g) label ->
  'h global -> 'd global

val scatter :
  ('a one, 'b one, 'c, 'd, 'e, 'f inp) role ->
  ('g list, 'e list, 'h, 'c, 'b, 'i) role ->
  ('i, ('j, 'a) scatter, 'f, 'j * 'g) label ->
  'h global -> 'd global

val choice_at :
  ('a one, 'b one, 'c, 'd, 'e, 'f) role ->
  ('b, 'g, 'h) disj ->
  ('g one, unit one, 'i, 'c, 'j, 'k) role * 'i global ->
  ('h one, unit one, 'm, 'c, 'n, 'o) role * 'm global ->
  'd global

val fix : ('g global -> 'g global) -> 'g global

val finish : ([ `cons of close one * 'a ] as 'a) global

val closed :
  (close one, close one, 'g, 'g, 'a, 'b) role ->
  'g global -> 'g global

val gen_with_param : Env.t -> 'a global -> 'a tup

val gen : 'a global -> 'a tup

val get_ch : ('a one, 'b, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a
val get_ch_list : ('a list, 'b, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a list
