type ('r,'ls) send = DummySend__
type ('r,'ls) sendmany = DummySend__
type ('r,'ls) receive = DummyReceive__
type close

exception RoleNotEnabled
 
type 'a one = One__ of 'a       
type 'a many = Many__ of 'a       
type conn = Conn
        
type _ e =
  (* slot contents *)
  ProtOne : 'a prot -> 'a prot one e
| ProtMany : 'a prot list -> 'a prot many e
        
and _ slots =
  Cons : 'x e lazy_t * 'xs slots lazy_t -> ('x * 'xs) slots
| Nil : unit slots

and (_,_,_,_) lens =
  | Fst  : ('a, 'b, ('a * 'xs) slots, ('b * 'xs) slots) lens
  | Next : ('a,'b, 'xs slots,'ys slots) lens
           -> ('a,'b, ('x * 'xs) slots, ('x * 'ys) slots) lens

and _ prot =
  | Send :
      'r * 'ls
      -> ('r, 'ls) send prot
  | Receive :
      'r * 'ls Lwt.t list
      -> ('r, 'ls) receive prot
  | Close : close prot
  | DummyReceive :
      ('r, 'ls) receive prot

let protone : type t. t prot one e -> t prot = function
    ProtOne p -> p

let protmany : type t. t prot many e -> t prot list = function
    ProtMany p -> p
  
let slot_tail : type hd tl. (hd * tl) slots lazy_t -> tl slots lazy_t = fun sl ->
  lazy begin
      match sl with
      | lazy (Cons(_,lazy tl)) -> tl
    end
  

let slot_head : type hd tl. (hd * tl) slots lazy_t -> hd e lazy_t = fun sl ->
  lazy begin
      match sl with
      | lazy (Cons(lazy hd,_)) -> hd
    end
  
let rec lens_get : type a b xs ys. (a, b, xs, ys) lens -> xs lazy_t -> a e lazy_t = fun ln xs ->
  match ln with
  | Fst -> slot_head xs
  | Next ln' -> lens_get ln' (slot_tail xs)

let lens_get_ ln s = Lazy.force (lens_get ln s)

let rec lens_put : type a b xs ys. (a,b,xs,ys) lens -> xs lazy_t -> b e lazy_t -> ys lazy_t =
  fun ln xs b ->
  match ln with
  | Fst -> lazy (Cons(b, slot_tail xs))
  | Next ln' ->
     lazy
       begin match xs with
       | lazy (Cons(a, xs')) -> Cons(a, lens_put ln' xs' b)
       end

let lens_put_ ln s v = lens_put ln s (Lazy.from_val v)

type ('r, 'v1, 'v2, 's1, 's2) role =
  {role:'r;
   lens:('v1, 'v2, 's1, 's2) lens;
   }
     
let send : 'r 'ls 'v 's.
  'r ->
  ((< .. > as 'ls) -> 'v -> 's prot) ->
  'v ->
  ('r, 'ls) send prot ->
  's prot =
  fun _ sel v (Send (_,ls)) ->
  let s = sel ls v in
  s
     
let receive : 'r 'ls.
  'r ->
  ('r, 'ls) receive prot -> 'ls Lwt.t =
  fun _ s ->
  match s with
  | Receive(_, lss) ->
     Lwt.choose lss
  | DummyReceive ->
     failwith "DummyReceive encountered" 

let close Close = ()

module Internal = struct
  
  let merge : type t. t prot -> t prot -> t prot = fun x y ->
    match x, y with
    | Send _, Send _ ->
       raise RoleNotEnabled
    | Receive (r, xs), Receive (_, ys) ->
       Receive (r, xs @ ys)
    | Receive (r, xs), DummyReceive ->
       Receive (r, xs)
    | DummyReceive, Receive (r, xs) ->
       Receive (r, xs)
    | DummyReceive, DummyReceive ->
       DummyReceive
    | Close, Close ->
       Close
end
