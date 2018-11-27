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

open Base

type ('r, 'v1, 'v2, 's1, 's2) role =
  {role:'r;
   lens:('v1, 'v2, 's1, 's2) lens;
   }
     
let send : 'r 'k 'ks 'ls 'v 's.
  ('r, 'k conn, _, 'ks, _) role ->
  ((< .. > as 'ls) -> 'v -> ('ks,'s) sess) ->
  'v ->
  ('ks, ('r, 'k, 'ls) send) sess ->
  ('ks,'s) sess =
  fun {lens;_} sel v (Sess (ks, Send (_,ls))) ->
  let k = lens_get lens ks in
  let ls = ls k ks in
  let s = sel ls v in
  s

let request : 'r 'k 'ks 'ks2 'v 's.
      ('r, unit, 'k conn, 'ks, 'ks2) role ->
      ('ls -> 'v -> ('ks2, 's) sess) ->
      'v ->
      'k conn ->
      ('ks, ('ks2, 'r, 'k, 'ls) request) sess ->
      ('ks2, 's) sess =
  fun {lens;_} sel v k (Sess (ks, Request (r, ls))) ->
  let ks2 = lens_put lens ks k in
  let ls = ls k ks2 in
  let s = sel ls v in
  s

let receive : 'r 'k 'ks 'ls.
  ('r, 'k conn, _, 'ks, _) role ->
  ('ks, ('r, 'k, 'ls) receive) sess -> 'ls Lwt.t =
  fun {lens;_} (Sess (ks, s)) ->
  match s with
  | Receive(_, lss) ->
     Lwt.choose (List.map (fun ls -> ls (lens_get lens ks) ks) lss)
  | DummyReceive ->
     failwith "DummyReceive encountered" 

let accept : 'r 'k 'ks 'ks2 'ls.
      ('r, unit, 'k conn, 'ks, 'ks2) role ->
      'k conn ->
      ('ks, ('ks2, 'r, 'k, 'ls) accept) sess -> 'ls Lwt.t =
  fun {lens;_} k (Sess (ks, Accept (r, lss))) ->
  let ks2 = lens_put lens ks k in
  Lwt.choose (List.map (fun ls -> ls k ks2) lss)
  
let disconnect :
      ('r, 'k, unit, 'ks, 'ks2) role ->
      ('ks, ('ks2, 'r, 'k, 's) disconnect) sess -> ('ks2, 's) sess =
  fun {lens;_} (Sess (ks, Disconnect (r, cont))) ->
  let ks2 = lens_put lens ks () in
  cont ks2

let close (Sess (_,_)) = ()

module Internal = struct
  
  let merge : type t. ('ks,t) prot -> ('ks,t) prot -> ('ks,t) prot = fun x y ->
    match x, y with
    | Send _, Send _ ->
       raise RoleNotEnabled
    | Request (_,_), Request (_,_) ->
       raise RoleNotEnabled
    | Disconnect (_, _), Disconnect (_, _) ->
       raise RoleNotEnabled
    | Receive (r, xs), Receive (_, ys) ->
       Receive (r, xs @ ys)
    | Receive (r, xs), DummyReceive ->
       Receive (r, xs)
    | DummyReceive, Receive (r, xs) ->
       Receive (r, xs)
    | DummyReceive, DummyReceive ->
       DummyReceive
    | Accept (r, xs), Accept (_, ys) ->
       Accept (r, xs @ ys)
    | Close, Close ->
       Close
end
