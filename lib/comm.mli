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
val send_many : ('v, 't) scatter -> (int -> 'v) -> 't IO.io
val receive_many : 'var gather -> 'var IO.io

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

val finish_with_multirole :
  at:(close one, close list, [ `cons of close one * 'a ] as 'a, 'g, _, _) role ->
  'g global

val with_multirole :
  at:(close one, close list, 'g0, 'g1, 'a, 'b) role ->
  'g0 global -> 'g1 global

val closed_at :
  (close one, close one, 'g, 'g, 'a, 'b) role ->
  'g global -> 'g global

val closed_list_at :
  (close list, close list, 'g, 'g, 'a, 'b) role ->
  'g global -> 'g global

val gen_with_env : Env.t -> 'a global -> 'a tup

val gen : 'a global -> 'a tup

val gen_mult : int list -> 'a global -> 'a tup

val get_ch : ('a one, 'b, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a

val get_ch_list : ('a list, 'b, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a list

type 'a ty

val get_ty : ('a one, 'b, 'c, 'd, 'e, 'f) role -> 'c global -> 'a ty

val get_ty_ : ('a one, 'b, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a ty

val get_ty_list : ('a list, 'b, 'c, 'd, 'e, 'f) role -> 'c global -> 'a ty

val get_ty_list_ : ('a list, 'b, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a ty

val (>:) :
  ('obj,('v, 'epA) out, 'var, 'v * 'epB) label ->
  'v ty ->
  ('obj,('v, 'epA) out, 'var, 'v * 'epB) label

val (>>:) :
  ('obj,('v, 'epA) scatter, 'var, 'v * 'epB) label ->
  'v ty ->
  ('obj,('v, 'epA) scatter, 'var, 'v * 'epB) label
    
val effective_length : 't tup -> int

val env : 't tup -> Env.t
