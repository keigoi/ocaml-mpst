open Base
open Session

type ('ka,'kb,'v) channel =
  {sender: 'ka conn -> 'v -> unit;
   receiver: 'kb conn -> 'v Lwt.t}

type ('la,'lb,'ca,'cb,'ka,'kb,'v) label =
  {channel: ('ka,'kb,'v) channel;
   select_label: ('v -> 'ca) -> 'la;
   offer_label: 'v * 'cb -> 'lb}

(** scribble-style, labelled value transmission (a --> b) label @@ cont  *)
val (-->) :
  ('ra, ('ksa, 'sa) prot, ('ksa, ('rb, 'ka, 'la) send) prot, 'c0, 'c1) role ->
  ('rb, ('ksb, 'sb) prot, ('ksb, ('ra, 'kb, 'lb) receive) prot, 'c1, 'c2) role ->
  ('la, 'lb, ('ksa,'sa) sess, ('ksb,'sb) sess, 'ka, 'kb, _) label ->
  'c0 lazy_t -> 'c2 lazy_t

(** explicit connection. use like ((a,a) --> (b,b)) label @@ cont *)
val (-!->) :
  ('ra, ('ksa2, 'sa) prot, ('ksa, ('ksa2, 'rb, 'ka dist, 'la) request) prot, 'c0, 'c1) role *
    ('ra, unit, 'kb dist conn, 'ksb, 'ksb2) role ->
  ('rb, ('ksb2, 'sb) prot, ('ksb, ('ksb2, 'ra, 'kb dist, 'lb) accept) prot, 'c1, 'c2) role *
    ('rb, unit, 'ka dist conn, 'ksa, 'ksa2) role ->
  ('la, 'lb, ('ksa2,'sa) sess, ('ksb2,'sb) sess, 'ka dist, 'kb dist, 'v) label ->
  'c0 lazy_t -> 'c2 lazy_t

(** explicit disconnection discon a b @@ cont *)
val discon :
  ('ra, ('ksa2, 'sa) prot, ('ksa, ('ksa2, 'rb, 'ka dist, 'sa) disconnect) prot, 'c0, 'c1) role *
    ('ra, 'kb dist conn, unit, 'ksb, 'ksb2) role ->
  ('rb, ('ksb2, 'sb) prot, ('ksb, ('ksb2, 'ra, 'kb dist, 'sb) disconnect) prot, 'c1, 'c2) role *
    ('rb, 'ka dist conn, unit, 'ksa, 'ksa2) role ->
  'c0 lazy_t -> 'c2 lazy_t


(** dummy reception for non-liveness *)
val dummy_receive :
  (_, _, (_, (_, _, _) receive) prot, 'c0, 'c1) role ->
  'c0 lazy_t ->
  'c1 lazy_t

(* dummy closing for non-liveness *)
val dummy_close :
  ('ra, _, (_, close) prot, 'c0, 'c1) role -> 'c0 lazy_t -> 'c1 lazy_t

type ('l, 'r, 'lr) label_merge =
  {label_merge: 'l -> 'r -> 'lr}

val choice_at :
  ('c1 lazy_t -> 'c1 lazy_t -> 'c1 lazy_t) ->
  ('ra, ('ks, close) prot, ('ks, ('rb, 'k, 'lr) send) prot, 'c1, 'c2) role ->
  ('l, 'r, 'lr) label_merge ->
  ('ra, ('ks, ('rb, 'k, 'l) send) prot, ('ks, close) prot, 'c0l, 'c1) role * 'c0l lazy_t ->
  ('ra, ('ks, ('rb, 'k, 'r) send) prot, ('ks, close) prot, 'c0r, 'c1) role * 'c0r lazy_t ->
  'c2 lazy_t

val loop : 'c lazy_t lazy_t -> 'c lazy_t
