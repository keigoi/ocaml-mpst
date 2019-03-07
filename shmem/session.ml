type ('r,'ls) send = DummySend__
type ('r,'ls) sendmany = DummySend__
type ('r,'ls) receive = DummyReceive__
type close

type 'k dist = DummyDist__

exception RoleNotEnabled

type _ slots =
  ConsProt : 'x prot lazy_t * 'xs slots lazy_t -> ('x prot * 'xs) slots
| ConsList : 'x prot list lazy_t * 'xs slots lazy_t -> ('x prot list * 'xs) slots
| Nil : unit slots

and (_,_,_,_) lens =
  | FstProt  : ('a prot, 'b prot, ('a prot * 'xs) slots, ('b prot * 'xs) slots) lens
  | FstList  : ('a prot list, 'b prot list, ('a prot list * 'xs) slots, ('b prot list * 'xs) slots) lens
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

let slot_tail : type hd tl. (hd * tl) slots lazy_t -> tl slots lazy_t = fun sl ->
  lazy begin
      match sl with
      | lazy (ConsProt(_,lazy tl)) -> tl
      | lazy (ConsList(_,lazy tl)) -> tl
    end
  

let slot_head_prot : type hd tl. (hd prot * tl) slots lazy_t -> hd prot lazy_t = fun sl ->
  lazy begin
      match sl with
      | lazy (ConsProt(lazy hd,_)) -> hd
    end

let slot_head_list : type hd tl. (hd prot list * tl) slots lazy_t -> hd prot list lazy_t = fun sl ->
  lazy begin
      match sl with
      | lazy (ConsList(lazy hd,_)) -> hd
    end
  
let rec lens_get : type a b xs ys. (a, b, xs, ys) lens -> xs lazy_t -> a lazy_t = fun ln xs ->
  match ln with
  | FstProt -> slot_head_prot xs
  | FstList -> slot_head_list xs
  | Next ln' -> lens_get ln' (slot_tail xs)

let lens_get_ ln s = Lazy.force (lens_get ln s)

let rec lens_put : type a b xs ys. (a,b,xs,ys) lens -> xs lazy_t -> b lazy_t -> ys lazy_t =
  fun ln xs b ->
  match ln with
  | FstProt -> lazy (ConsProt(b, slot_tail xs))
  | FstList -> lazy (ConsList(b, slot_tail xs))
  | Next ln' ->
     lazy
       begin match xs with
       | lazy (ConsProt(a, xs')) -> ConsProt(a, lens_put ln' xs' b)
       | lazy (ConsList(a, xs')) -> ConsList(a, lens_put ln' xs' b)
       end

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
