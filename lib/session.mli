type ('ss, 'r) send = DummySelect
type ('ss, 'r) receive = DummyOffer
type ('ks,'ls,'r) request = DummyRequest
type ('ks,'ls,'r) accept = DummyAccept
type ('ks,'ls,'r) disconnect = DummyDisconnect
type close
type ('k, 'r) conn = DummyConn

type (_,_) prot =
  | Select :
      'r * ('k -> 'ks -> 'ls)
      -> ('ks, ('ls, ('k,'r) conn) send) prot
  | Offer :
      'r * ('k -> 'ks -> 'ls Lwt.t) list
      -> ('ks, ('ls, ('k,'r) conn) receive) prot
  | Request :
      'r * ('k -> 'ks2 -> 'ls)
      -> ('ks, ('ks2, 'ls, ('k, 'r) conn) request) prot
  | Accept :
      'r * ('k -> 'ks2 -> 'ls Lwt.t) list
      -> ('ks, ('ks2, 'ls, ('k, 'r) conn) accept) prot
  | Disconnect :
      'r * ('ks2 -> ('ks2, 's) sess) ->
      ('ks, ('ks2, 's, ('k, 'r) conn) disconnect) prot
  | Close : ('ks, close) prot
and ('ks,'c) sess =
  Sess of 'ks * ('ks, 'c) prot

type ('r, 'v1, 'v2, 's1, 's2) role =
  {role:'r;
   lens:('v1, 'v2, 's1, 's2) Base.lens}

val send :
  ('r, 'k, _, 'ks, _) role ->
  ((< .. > as 'ls) -> 'v -> ('ks,'s) sess) -> 'v -> ('ks, ('ls, ('k, 'r) conn) send) sess -> ('ks,'s) sess

val receive :
  ('r, 'k, _, 'ks, _) role ->
  ('ks, (([>] as 'ls), ('k, 'r) conn) receive) sess -> 'ls Lwt.t

val close : ('a, close) sess -> unit

val request :
  ('r, unit, 'k, 'ks, 'ks2) role ->
  ('ls -> 'v -> ('ks2, 's) sess) ->
  'v ->
  'k ->
  ('ks, ('ks2, 'ls, ('k, 'r) conn) request) sess ->
  ('ks2, 's) sess

val accept :
  ('r, unit, 'k, 'ks, 'ks2) role ->
  'k ->
  ('ks, ('ks2, 'ls, ('k, 'r) conn) accept) sess -> 'ls Lwt.t

val disconnect :
  ('r, 'k, unit, 'ks, 'ks2) role ->
  ('ks, ('ks2, 's, ('k, 'r) conn) disconnect) sess -> ('ks2, 's) sess

module Internal :
  sig val merge : ('ks, 't) prot -> ('ks, 't) prot -> ('ks, 't) prot end
