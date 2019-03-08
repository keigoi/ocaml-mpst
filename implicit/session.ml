type ('r,'ls) send = DummySend__
type ('r,'ls) sendmany = DummySend__
type ('r,'ls) receive = DummyReceive__
type close

exception RoleNotEnabled
 
type 'a one = One__ of 'a       
type 'a many = Many__ of 'a       
type conn = Conn

(* TODO dummy *)
let rawsend (k:conn) v = ()
let rawreceive (k:conn) =
  let st, _ = Lwt_stream.create () in Lwt_stream.next st

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

type _ e =
  (* slot contents *)
  One : 'a sess -> 'a sess one e
| Many : 'a sess list -> 'a sess many e
        
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

and 'p sess =
  Sess of ConnTable.t * 'p prot

let unone : type t. t sess one e -> t sess = function
    One p -> p

let unmany : type t. t sess many e -> t sess list = function
    Many p -> p
  
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

let send : 'r 'ls 'v 's.
  'r ->
  ((< .. > as 'ls) -> 'v -> 's sess) ->
  'v ->
  ('r, 'ls) send sess ->
  's sess =
  fun _ sel v (Sess (ks,Send (_,ls))) ->
  let s = sel ls v in
  s
     
let receive : 'r 'ls.
  'r ->
  ('r, 'ls) receive sess -> 'ls Lwt.t =
  fun _ s ->
  match s with
  | Sess(_,Receive(_, lss)) ->
     Lwt.choose lss
  | Sess(_,DummyReceive) ->
     failwith "DummyReceive encountered" 

let close (Sess(_,Close)) = ()

module Internal = struct
  
  let merge_prot : type t. t prot -> t prot -> t prot = fun x y ->
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

  let merge = fun (Sess(ksx,x)) (Sess(_,y)) ->
    Sess(ksx,merge_prot x y)
end
