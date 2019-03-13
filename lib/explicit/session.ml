open Mpst_base

type ('r,'k,'ls) send = Send__
type ('r,'k,'ls) receive = Receive__
type ('ks,'r,'k,'ls) request = Request__
type ('ks,'r,'k,'ls) accept = Accept__
type ('ks,'r,'k,'ls) disconnect = Disconnect__
type close

let lv = Lazy.from_val
let lf = Lazy.force

type 'k conn = Conn__

type (_,_) prot =
  | Send :
      'r * ('k -> 'ks -> 'ls)
      -> ('ks, ('r, 'k, 'ls) send) prot
  | Receive :
      'r * ('k -> 'ks -> 'ls Lwt.t) list
      -> ('ks, ('r, 'k, 'ls) receive) prot
  | Request :
      'r * ('k -> 'ks2 -> 'ls)
      -> ('ks, ('ks2, 'r, 'k, 'ls) request) prot
  | Accept :
      'r * ('k -> 'ks2 -> 'ls Lwt.t) list
      -> ('ks, ('ks2, 'r, 'k, 'ls) accept) prot
  | Disconnect :
      'r * ('ks2 -> ('ks2, 's) sess) ->
      ('ks, ('ks2, 'r, 'k, 's) disconnect) prot
  | Close : ('ks, close) prot
  | DummyReceive :
      ('ks, ('r, 'k, 'ls) receive) prot
and ('ks,'c) sess =
  Sess of 'ks * ('ks, 'c) prot

type _ e =
  (* slot contents *)
  Prot : ('a,'b) prot -> ('a,'b) prot e
| Conn : 'k -> 'k conn e
| Unit : unit e

include Mpst_base.Lens.Make(struct type 't u = 't e end)

let unprot : type t u. (t,u) prot e -> (t,u) prot = function
    Prot p -> p

let unconn : type k. k conn e -> k = function
    Conn p -> p
  
let slot_head_prot : type hdx hdy tl. ((hdx, hdy) prot * tl) slots lazy_t -> (hdx,hdy) prot lazy_t = fun sl ->
  lazy begin
      match sl with
      | lazy (Cons(lazy (Prot(hd)),_)) -> hd
    end

let slot_head_conn : type k tl. (k conn * tl) slots lazy_t -> k lazy_t = fun sl ->
  lazy begin
      match sl with
      | lazy (Cons(lazy (Conn(hd)),_)) -> hd
    end

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
  let (Conn k) = lens_get_ lens (lv ks) in
  let ls = ls k ks in
  let s = sel ls v in
  s

let request : 'r 'k 'ks 'ks2 'v 's.
      ('r, unit, 'k conn, 'ks, 'ks2) role ->
      ('ls -> 'v -> ('ks2, 's) sess) ->
      'v ->
      'k ->
      ('ks, ('ks2, 'r, 'k, 'ls) request) sess ->
      ('ks2, 's) sess =
  fun {lens;_} sel v k (Sess (ks, Request (r, ls))) ->
  let ks2 = lens_put_ lens (lv ks) (Conn k) in
  let ls = ls k (lf ks2) in
  let s = sel ls v in
  s

let receive : 'r 'k 'ks 'ls.
  ('r, 'k conn, _, 'ks, _) role ->
  ('ks, ('r, 'k, 'ls) receive) sess -> 'ls Lwt.t =
  fun {lens;_} (Sess (ks, s)) ->
  match s with
  | Receive(_, lss) ->
     Lwt.choose (List.map (fun ls -> ls (unconn (lens_get_ lens (lv ks))) ks) lss)
  | DummyReceive ->
     failwith "DummyReceive encountered" 

let accept : 'r 'k 'ks 'ks2 'ls.
      ('r, unit, 'k conn, 'ks, 'ks2) role ->
      'k ->
      ('ks, ('ks2, 'r, 'k, 'ls) accept) sess -> 'ls Lwt.t =
  fun {lens;_} k (Sess (ks, Accept (r, lss))) ->
  let ks2 = lens_put_ lens (lv ks) (Conn k) in
  Lwt.choose (List.map (fun ls -> ls k (lf ks2)) lss)
  
let disconnect :
      ('r, 'k conn, unit, 'ks, 'ks2) role ->
      ('ks, ('ks2, 'r, 'k, 's) disconnect) sess -> ('ks2, 's) sess =
  fun {lens;_} (Sess (ks, Disconnect (r, cont))) ->
  let ks2 = lens_put_ lens (lv ks) Unit in
  cont (lf ks2)

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
