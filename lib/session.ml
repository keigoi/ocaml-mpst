type ('ls,'r) send = DummySend__
type ('ls,'r) receive = DummyReceive__
type ('ks,'ls,'r) request = DummyRequest__
type ('ks,'ls,'r) accept = DummyAccept__
type ('ks,'ls,'r) disconnect = DummyDisconnect__
type close
type ('k, 'r) conn = DummyConn__

exception RoleNotEnabled

type (_,_) prot =
  | Send :
      'r * ('k -> 'ks -> 'ls)
      -> ('ks, ('ls, ('k,'r) conn) send) prot
  | Receive :
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
  | DummyReceive :
      ('ks, ('ls, ('k, 'r) conn) receive) prot
and ('ks,'c) sess =
  Sess of 'ks * ('ks, 'c) prot

open Base

type ('r, 'v1, 'v2, 's1, 's2) role =
  {role:'r;
   lens:('v1, 'v2, 's1, 's2) lens;
   }
     
let send : 'r 'k 'ks 'ls 'v 's.
  ('r, 'k, _, 'ks, _) role ->
  ((< .. > as 'ls) -> 'v -> ('ks,'s) sess) ->
  'v ->
  ('ks, ('ls, ('k, 'r) conn) send) sess ->
  ('ks,'s) sess =
  fun {lens;_} sel v (Sess (ks, Send (_,ls))) ->
  let k = lens_get lens ks in
  let ls = ls k ks in
  let s = sel ls v in
  s

let request : 'r 'k 'ks 'ks2 'v 's.
      ('r, unit, 'k, 'ks, 'ks2) role ->
      ('ls -> 'v -> ('ks2, 's) sess) ->
      'v ->
      'k ->
      ('ks, ('ks2, 'ls, ('k, 'r) conn) request) sess ->
      ('ks2, 's) sess =
  fun {lens;_} sel v k (Sess (ks, Request (r, ls))) ->
  let ks2 = lens_put lens ks k in
  let ls = ls k ks2 in
  let s = sel ls v in
  s

let receive : 'r 'k 'ks 'ls.
  ('r, 'k, _, 'ks, _) role ->
  ('ks, ('ls, ('k, 'r) conn) receive) sess -> 'ls Lwt.t =
  fun {lens;_} (Sess (ks, s)) ->
  match s with
  | Receive(_, lss) ->
     Lwt.choose (List.map (fun ls -> ls (lens_get lens ks) ks) lss)
  | DummyReceive ->
     failwith "DummyReceive encountered" 

let accept : 'r 'k 'ks 'ks2 'ls.
      ('r, unit, 'k, 'ks, 'ks2) role ->
      'k ->
      ('ks, ('ks2, 'ls, ('k, 'r) conn) accept) sess -> 'ls Lwt.t =
  fun {lens;_} k (Sess (ks, Accept (r, lss))) ->
  let ks2 = lens_put lens ks k in
  Lwt.choose (List.map (fun ls -> ls k ks2) lss)
  
let disconnect :
      ('r, 'k, unit, 'ks, 'ks2) role ->
      ('ks, ('ks2, 's, ('k, 'r) conn) disconnect) sess -> ('ks2, 's) sess =
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
