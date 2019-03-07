type ('r,'ls) send = DummySend__
type ('r,'ls) sendmany = DummySend__
type ('r,'ls) receive = DummyReceive__
type close

type 'k dist = DummyDist__

exception RoleNotEnabled

type _ slots =
  ConsProt : 'x prot lazy_t * 'xs slots -> ('x prot * 'xs) slots
| ConsList : 'x prot list lazy_t * 'xs slots -> ('x prot list * 'xs) slots
| Nil : unit slots

and (_,_,_,_) lens =
  | FstProt  : ('a prot,'b prot, ('a prot * 'xs) slots, ('b prot * 'xs) slots) lens
  | FstList  : ('a prot list,'b prot list, ('a prot list * 'xs) slots, ('b prot list * 'xs) slots) lens
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

let rec lget : type a b xs ys. (a, b, xs, ys) lens -> xs -> a lazy_t = fun ln xs ->
  match ln,xs with
  | FstProt, ConsProt(a,_)        -> a
  | FstList, ConsList(a,_)        -> a
  | Next ln', ConsProt(_,xs') -> lget ln' xs'
  | Next ln', ConsList(_,xs') -> lget ln' xs'

let lens_get l s = lget l (Lazy.force s)

let rec lput : type a b xs ys. (a,b,xs,ys) lens -> xs -> b lazy_t -> ys =
  fun ln xs b ->
  match ln, xs with
  | FstProt, ConsProt(_, xs) -> ConsProt(b, xs)
  | FstList, ConsList(_, xs) -> ConsList(b, xs)
  | Next ln', ConsProt(a, xs') -> ConsProt(a, lput ln' xs' b)
  | Next ln', ConsList(a, xs') -> ConsList(a, lput ln' xs' b)

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
