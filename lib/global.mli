open Base
open Session

exception RoleNotEnabled

type 'xs mpst = Mpst of 'xs lazy_t

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
  ('ra, ('ksa, 'sa) prot, ('ksa, ('la, ('ka, 'rb) conn) send) prot, 'c0 mpst, 'c1 mpst) role ->
  ('rb, ('ksb, 'sb) prot, ('ksb, ('lb, ('kb, 'ra) conn) receive) prot, 'c1 mpst, 'c2 mpst) role ->
  ('la, 'lb, ('ksa,'sa) sess, ('ksb,'sb) sess, 'ka, 'kb, 'v) label ->
  'c0 mpst -> 'c2 mpst

val (-!->) :
  ('ra, ('ksa2, 'sa) prot, ('ksa, ('ksa2, 'la, ('ka, 'rb) conn) request) prot, 'c0 mpst, 'c1 mpst) role ->
  ('rb, ('ksb2, 'sb) prot, ('ksb, ('ksb2, 'lb, ('kb, 'ra) conn) accept) prot, 'c1 mpst, 'c2 mpst) role ->
  ('la, 'lb, ('ksa2,'sa) sess, ('ksb2,'sb) sess, 'ka, 'kb, 'v) label ->
  'c0 mpst -> 'c2 mpst

val discon :
  ('ra, ('ksa2, 'sa) prot, ('ksa, ('ksa2, 'sa, ('ka, 'rb) conn) disconnect) prot, 'c0 mpst, 'c1 mpst) role ->
  ('rb, ('ksb2, 'sb) prot, ('ksb, ('ksb2, 'sb, ('kb, 'ra) conn) disconnect) prot, 'c1 mpst, 'c2 mpst) role ->
  'c0 mpst -> 'c2 mpst

val choice_at :
  ('c1 -> 'c1 -> 'c1) ->
  ('ra, ('ks, close) prot, ('ks, ('lr, ('k, 'rb) conn) send) prot, 'c1, 'c2) role ->
  ('l, 'r, 'lr) label_merge ->
  ('ra, ('ks, ('l, ('k, 'rb) conn) send) prot, ('ks, close) prot, 'c0l, 'c1) role * 'c0l ->
  ('ra, ('ks, ('r, ('k, 'rb) conn) send) prot, ('ks, close) prot, 'c0r, 'c1) role * 'c0r ->
  'c2

val loop : 'c mpst lazy_t -> 'c mpst

