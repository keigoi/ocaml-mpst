type ('ls,'r) send = DummySelect
type ('ls,'r) receive = DummyOffer
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
   lens:('v1, 'v2, 's1, 's2) Base.lens;
   }
     
let send : 'r 'k 'ks 'ls 'v 's.
  ('r, 'k, _, 'ks, _) role ->
  ((< .. > as 'ls) -> 'v -> ('ks,'s) sess) ->
  'v ->
  ('ks, ('ls, ('k, 'r) conn) send) sess ->
  ('ks,'s) sess =
  fun {lens;_} sel v (Sess (ks, Select (_,ls))) ->
  let k = lens.get ks in
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
  let ks2 = lens.put ks k in
  let ls = ls k ks2 in
  let s = sel ls v in
  s

let receive : 'r 'k 'ks 'ls.
  ('r, 'k, _, 'ks, _) role ->
  ('ks, ('ls, ('k, 'r) conn) receive) sess -> 'ls Lwt.t =
  fun {lens;_} (Sess (ks, Offer(_, lss))) ->
  Lwt.choose (List.map (fun ls -> ls (lens.get ks) ks) lss)

let accept : 'r 'k 'ks 'ks2 'ls.
      ('r, unit, 'k, 'ks, 'ks2) role ->
      'k ->
      ('ks, ('ks2, 'ls, ('k, 'r) conn) accept) sess -> 'ls Lwt.t =
  fun {lens;_} k (Sess (ks, Accept (r, lss))) ->
  let ks2 = lens.put ks k in
  Lwt.choose (List.map (fun ls -> ls k ks2) lss)
  
let disconnect :
      ('r, 'k, unit, 'ks, 'ks2) role ->
      ('ks, ('ks2, 's, ('k, 'r) conn) disconnect) sess -> ('ks2, 's) sess =
  fun {lens;_} (Sess (ks, Disconnect (r, cont))) ->
  let ks2 = lens.put ks () in
  cont ks2

let close (Sess (_,_)) = ()

module Internal = struct
  
  let merge : type t. ('ks,t) prot -> ('ks,t) prot -> ('ks,t) prot = fun x y ->
    match x, y with
    | Select _, Select _ ->
       failwith "role not enabled"
    | Request (_,_), Request (_,_) ->
       failwith "role not enabled"
    | Disconnect (_, _), Disconnect (_, _) ->
       failwith "role not enabled"
    | Offer (r, xs), Offer (_, ys) ->
       Offer (r, xs @ ys)
    | Accept (r, xs), Accept (_, ys) ->
       Accept (r, xs @ ys)
    | Close, Close ->
       Close
end
