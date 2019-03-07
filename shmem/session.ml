type ('r,'ls) send = DummySend__
type ('r,'ls) sendmany = DummySend__
type ('r,'ls) receive = DummyReceive__
type close

exception RoleNotEnabled

type 'a conn = Conn of 'a        
        
type _ one =
  ProtOne : 'a prot -> 'a prot one
| ConnOne : 'a conn -> 'a conn one

and _ many =
  ProtMany : 'a prot list -> 'a prot many
| ConnMany : 'a conn list -> 'a conn many
        
and _ slots =
  (* this distinction between ConsOne/ConsMany is crucial for merging contexts in choice_at *)
  ConsOne : 'x one lazy_t * 'xs slots lazy_t -> ('x one * 'xs) slots
| ConsMany : 'x many lazy_t * 'xs slots lazy_t -> ('x many * 'xs) slots
| Nil : unit slots

and (_,_,_,_) lens =
  (* the elaboration on prot (or prot list) is crucial to make lens_put's source argument non-strict
     (which is in turn required for defining recursive protocols) *)
  | FstOne  : ('a one, 'b one, ('a one * 'xs) slots, ('b one * 'xs) slots) lens
  | FstMany  : ('a many, 'b many, ('a many * 'xs) slots, ('b many * 'xs) slots) lens
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

let protone : type t. t prot one -> t prot = function
    ProtOne p -> p

let protmany : type t. t prot many -> t prot list = function
    ProtMany p -> p
  
let slot_tail : type hd tl. (hd * tl) slots lazy_t -> tl slots lazy_t = fun sl ->
  lazy begin
      match sl with
      | lazy (ConsOne(_,lazy tl)) -> tl
      | lazy (ConsMany(_,lazy tl)) -> tl
    end
  

let slot_head_one : type hd tl. (hd one * tl) slots lazy_t -> hd one lazy_t = fun sl ->
  lazy begin
      match sl with
      | lazy (ConsOne(lazy hd,_)) -> hd
    end

let slot_head_many : type hd tl. (hd many * tl) slots lazy_t -> hd many lazy_t = fun sl ->
  lazy begin
      match sl with
      | lazy (ConsMany(lazy hd,_)) -> hd
    end
  
let rec lens_get : type a b xs ys. (a, b, xs, ys) lens -> xs lazy_t -> a lazy_t = fun ln xs ->
  match ln with
  | FstOne -> slot_head_one xs
  | FstMany -> slot_head_many xs
  | Next ln' -> lens_get ln' (slot_tail xs)

let lens_get_ ln s = Lazy.force (lens_get ln s)

let rec lens_put : type a b xs ys. (a,b,xs,ys) lens -> xs lazy_t -> b lazy_t -> ys lazy_t =
  fun ln xs b ->
  match ln with
  | FstOne -> lazy (ConsOne(b, slot_tail xs))
  | FstMany -> lazy (ConsMany(b, slot_tail xs))
  | Next ln' ->
     lazy
       begin match xs with
       | lazy (ConsOne(a, xs')) -> ConsOne(a, lens_put ln' xs' b)
       | lazy (ConsMany(a, xs')) -> ConsMany(a, lens_put ln' xs' b)
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
