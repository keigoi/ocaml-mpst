type ('r,'ls) send = DummySend__
type ('r,'ls) sendmany = DummySendMany__
type ('r,'ls) receive = DummyReceive__
type ('r,'ls) receivemany = DummyReceiveMany__
type close

exception RoleNotEnabled
 
type conn = Conn
type 't one = One__ of 't
type 't many = Many__ of 't
          
          
type _ ep =
  EPOne : conn * 'r -> 'r one ep
| EPMany : conn list * 'r -> 'r many ep
        
type _ slots =
  Cons : 'x prot lazy_t * 'xs slots lazy_t -> ('x prot * 'xs) slots
| Nil : unit slots

and (_,_,_,_) lens =
  | Fst  : ('a prot, 'b prot, ('a prot * 'xs) slots, ('b prot * 'xs) slots) lens
  | Next : ('a,'b, 'xs slots,'ys slots) lens
           -> ('a,'b, ('x * 'xs) slots, ('x * 'ys) slots) lens

and _ prot =
  | Send :
      'r * (conn -> 'ls)
      -> ('r, 'ls) send prot
  | SendMany :
      'r * (conn list -> 'ls)
      -> ('r, 'ls) sendmany prot
  | Receive :
      'r * (conn -> 'ls Lwt.t list)
      -> ('r, 'ls) receive prot
  | ReceiveMany :
      'r * (conn list -> 'ls Lwt.t list)
      -> ('r, 'ls) receivemany prot
  | Close : close prot
  | DummyReceive :
      ('r, 'ls) receive prot

module ConnTable : sig
  type t
  val create : unit -> t
  val get : t -> 'k -> conn
  val put : t -> 'k -> conn -> unit
end = struct
  type t = (Obj.t, conn) Hashtbl.t
  let create () = Hashtbl.create 42
  let get t k = Hashtbl.find t (Obj.repr k)
  let put t k v = Hashtbl.add t (Obj.repr k) v
end

let slot_tail : type hd tl. (hd * tl) slots lazy_t -> tl slots lazy_t = fun sl ->
  lazy begin
      match sl with
      | lazy (Cons(_,lazy tl)) -> tl
    end
  
let slot_head : type hd tl. (hd * tl) slots lazy_t -> hd lazy_t = fun sl ->
  lazy begin
      match sl with
      | lazy (Cons(lazy hd,_)) -> hd
    end
  
let rec lens_get : type a b xs ys. (a, b, xs, ys) lens -> xs lazy_t -> a lazy_t = fun ln xs ->
  match ln with
  | Fst -> slot_head xs
  | Next ln' -> lens_get ln' (slot_tail xs)

let lens_get_ ln s = Lazy.force (lens_get ln s)

let rec lens_put : type a b xs ys. (a,b,xs,ys) lens -> xs lazy_t -> b lazy_t -> ys lazy_t =
  fun ln xs b ->
  match ln with
  | Fst -> lazy (Cons(b, slot_tail xs))
  | Next ln' ->
     lazy
       begin match xs with
       | lazy (Cons(a, xs')) -> Cons(a, lens_put ln' xs' b)
       end

let lens_put_ ln s v = lens_put ln s (Lazy.from_val v)

let send : 'r 'ls 'v 's.
  'r one ep ->
  ((< .. > as 'ls) -> 'v -> 's prot) ->
  'v ->
  ('r, 'ls) send prot ->
  's prot =
  fun (EPOne(k,_)) sel v (Send (_,f)) ->
  let s = sel (f k) v in
  s

let multicast : 'r 'ls 'v 's.
  'r many ep ->
  ((< .. > as 'ls) -> (int -> 'v) -> 's prot) ->
  (int -> 'v) ->
  ('r, 'ls) sendmany prot ->
  's prot =
  fun (EPMany(ks,_)) sel v (SendMany (_,f)) ->
  let s = sel (f ks) v in
  s
     
let receive : 'r 'ls.
  'r one ep ->
  ('r, 'ls) receive prot -> 'ls Lwt.t =
  fun (EPOne(k,_)) s ->
  match s with
  | Receive(_, f) ->
     Lwt.choose (f k)
  | DummyReceive ->
     failwith "DummyReceive encountered" 

let gather : 'r 'ls.
  'r many ep ->
  ('r, 'ls) receivemany prot -> 'ls Lwt.t =
  fun (EPMany(ks,_)) s ->
  match s with
  | ReceiveMany(_, f) ->
     Lwt.choose (f ks)

let close Close = ()

module Internal = struct
  
  let merge : type t. t prot -> t prot -> t prot = fun x y ->
    match x, y with
    | Send _, Send _ ->
       raise RoleNotEnabled
    | SendMany _, SendMany _ ->
       raise RoleNotEnabled
    | Receive (r, xs), Receive (_, ys) ->
       Receive (r, fun k -> xs k @ ys k)
    | ReceiveMany (r, xs), ReceiveMany (_, ys) ->
       ReceiveMany (r, fun ks -> xs ks @ ys ks)
    | Receive (r, xs), DummyReceive ->
       Receive (r, xs)
    | DummyReceive, Receive (r, xs) ->
       Receive (r, xs)
    | DummyReceive, DummyReceive ->
       DummyReceive
    | Close, Close ->
       Close
end
