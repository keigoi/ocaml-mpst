type ('r,'ls) send = DummySend__
type ('r,'ls) receive = DummyReceive__
type close

type 'k dist = DummyDist__

exception RoleNotEnabled

type _ slots =
  Cons : 'x prot lazy_t * 'xs slots -> ('x * 'xs) slots
| Nil : unit slots

and (_,_,_,_) lens =
  | Fst  : ('a,'b, ('a * 'xs) slots, ('b * 'xs) slots) lens
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

let rec lget : type a b xs ys. (a, b, xs, ys) lens -> xs -> a prot = fun ln xs ->
  match ln,xs with
  | Fst, Cons(a,_)        -> Lazy.force a
  | Next ln', Cons(_,xs') -> lget ln' xs'

let lens_get l s = lget l (Lazy.force s)
                       
let rec lput : type a b xs ys. (a,b,xs,ys) lens -> xs -> b prot -> ys = fun ln xs b ->
  match ln, xs with
  | Fst, Cons(_, xs) -> Cons(Lazy.from_val b, xs)
  | Next ln', Cons(a, xs') -> Cons(a, lput ln' xs' b)

let lens_put l s v = lazy (lput l (Lazy.force s) v)

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
