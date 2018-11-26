open Base
open Session

type ('ka,'kb,'v) channel =
  {sender: 'ka -> 'v -> unit;
   receiver: 'kb -> 'v Lwt.t}

type ('la,'lb,'ca,'cb,'ka,'kb,'v) label =
  {make_channel: unit -> ('ka,'kb,'v) channel;
   select_label: ('v -> 'ca) -> 'la;
   offer_label: 'v * 'cb -> 'lb}

type ('l, 'r, 'lr) label_merge =
  {label_merge: 'l -> 'r -> 'lr}

val (-->) :
  ('ra, ('ksa, 'sa) prot, ('ksa, ('la, ('ka, 'rb) conn) send) prot, 'c0, 'c1) role ->
  ('rb, ('ksb, 'sb) prot, ('ksb, ('lb, ('kb, 'ra) conn) receive) prot, 'c1, 'c2) role ->
  ('la, 'lb, ('ksa,'sa) sess, ('ksb,'sb) sess, 'ka, 'kb, _) label ->
  'c0 lazy_t -> 'c2 lazy_t

val (-!->) :
  ('ra, ('ksa2, 'sa) prot, ('ksa, ('ksa2, 'la, ('ka, 'rb) conn) request) prot, 'c0, 'c1) role *
    ('ra, unit, 'kb, 'ksb, 'ksb2) role ->
  ('rb, ('ksb2, 'sb) prot, ('ksb, ('ksb2, 'lb, ('kb, 'ra) conn) accept) prot, 'c1, 'c2) role *
    ('rb, unit, 'ka, 'ksa, 'ksa2) role ->
  ('la, 'lb, ('ksa2,'sa) sess, ('ksb2,'sb) sess, 'ka, 'kb, 'v) label ->
  'c0 lazy_t -> 'c2 lazy_t

val discon :
  ('ra, ('ksa2, 'sa) prot, ('ksa, ('ksa2, 'sa, ('ka, 'rb) conn) disconnect) prot, 'c0, 'c1) role ->
  ('rb, ('ksb2, 'sb) prot, ('ksb, ('ksb2, 'sb, ('kb, 'ra) conn) disconnect) prot, 'c1, 'c2) role ->
  'c0 lazy_t -> 'c2 lazy_t

val dummy_receive :
  (_, _, (_, (_, (_, _) conn) receive) prot, 'c0, 'c1) role ->
  'c0 lazy_t ->
  'c1 lazy_t

val dummy_close :
  ('ra, _, (_, close) prot, 'c0, 'c1) role -> 'c0 lazy_t -> 'c1 lazy_t

val choice_at :
  ('c1 lazy_t -> 'c1 lazy_t -> 'c1 lazy_t) ->
  ('ra, ('ks, close) prot, ('ks, ('lr, ('k, 'rb) conn) send) prot, 'c1, 'c2) role ->
  ('l, 'r, 'lr) label_merge ->
  ('ra, ('ks, ('l, ('k, 'rb) conn) send) prot, ('ks, close) prot, 'c0l, 'c1) role * 'c0l lazy_t ->
  ('ra, ('ks, ('r, ('k, 'rb) conn) send) prot, ('ks, close) prot, 'c0r, 'c1) role * 'c0r lazy_t ->
  'c2 lazy_t

val loop : 'c lazy_t lazy_t -> 'c lazy_t
