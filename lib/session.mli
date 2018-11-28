type ('r,'k,'ls) send = DummySend__
type ('r,'k,'ls) receive = DummyReceive__
type ('ks,'r,'k,'ls) request = DummyRequest__
type ('ks,'r,'k,'ls) accept = DummyAccept__
type ('ks,'r,'k,'ls) disconnect = DummyDisconnect__
type close

type memory = DummyMem__
type 'k dist = DummyDist__

type 'k conn =
  Memory : memory conn
| Conn : 'k -> 'k dist conn

exception RoleNotEnabled

type (_,_) prot =
  | Send :
      'r * ('k conn -> 'ks -> 'ls)
      -> ('ks, ('r, 'k, 'ls) send) prot
  | Receive :
      'r * ('k conn -> 'ks -> 'ls Lwt.t) list
      -> ('ks, ('r, 'k, 'ls) receive) prot
  | Request :
      'r * ('k conn -> 'ks2 -> 'ls)
      -> ('ks, ('ks2, 'r, 'k, 'ls) request) prot
  | Accept :
      'r * ('k conn -> 'ks2 -> 'ls Lwt.t) list
      -> ('ks, ('ks2, 'r, 'k, 'ls) accept) prot
  | Disconnect :
      'r * ('ks2 -> ('ks2, 's) sess) ->
      ('ks, ('ks2, 'r, 'k, 's) disconnect) prot
  | Close : ('ks, close) prot
  | DummyReceive :
      ('ks, ('r, 'k, 'ls) receive) prot
and ('ks,'c) sess =
  Sess of 'ks * ('ks, 'c) prot

type ('r, 'v1, 'v2, 's1, 's2) role =
  {role:'r;
   lens:('v1, 'v2, 's1, 's2) Base.lens}

val send :
  ('r, 'k conn, _, 'ks, _) role ->
  ((< .. > as 'ls) -> 'v -> ('ks,'s) sess) ->
  'v -> ('ks, ('r, 'k, 'ls) send) sess ->
  ('ks,'s) sess

val receive :
  ('r, 'k conn, _, 'ks, _) role ->
  ('ks, ('r, 'k, ([>] as 'ls)) receive) sess ->
  'ls Lwt.t

val close : ('a, close) sess -> unit

val request :
  ('r, unit, 'k dist conn, 'ks, 'ks2) role ->
  ('ls -> 'v -> ('ks2, 's) sess) ->
  'v ->
  'k ->
  ('ks, ('ks2, 'r, 'k dist, 'ls) request) sess ->
  ('ks2, 's) sess

val accept :
  ('r, unit, 'k dist conn, 'ks, 'ks2) role ->
  'k ->
  ('ks, ('ks2, 'r, 'k dist, 'ls) accept) sess ->
  'ls Lwt.t

val disconnect :
  ('r, 'k dist conn, unit, 'ks, 'ks2) role ->
  ('ks, ('ks2, 'r, 'k dist, 's) disconnect) sess ->
  ('ks2, 's) sess

module Internal :
  sig val merge : ('ks, 't) prot -> ('ks, 't) prot -> ('ks, 't) prot end
