open Mpst_base
type ('r,'ls) send = Send__
type ('r,'ls) sendmany = SendMany__
type ('r,'ls) receive = Receive__
type close
        
type _ prot =
  | Send :
      'r * 'ls
      -> ('r, 'ls) send prot
  | Receive :
      'r * (unit -> 'ls Lwt.t list)
      -> ('r, 'ls) receive prot
  | Close : close prot
  | DummyReceive :
      ('r, 'ls) receive prot

type 'p sess = 'p prot
     
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
     Lwt.choose (lss ())
  | DummyReceive ->
     failwith "DummyReceive encountered" 

let close Close = ()

module Internal = struct
  
  let merge : type t. t prot -> t prot -> t prot = fun x y ->
    match x, y with
    | Send _, Send _ ->
       raise RoleNotEnabled
    | Receive (r, xs), Receive (_, ys) ->
       Receive (r, (fun () -> xs () @ ys ()))
    | Receive (r, xs), DummyReceive ->
       Receive (r, xs)
    | DummyReceive, Receive (r, xs) ->
       Receive (r, xs)
    | DummyReceive, DummyReceive ->
       DummyReceive
    | Close, Close ->
       Close
end
