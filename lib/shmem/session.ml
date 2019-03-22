open Mpst_base

module Make(F:S.FLAG) = struct

module Flag = F

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

type 'p sess =
  {once:Flag.t; prot:'p prot}

let send : 'r 'ls 'v 's.
  'r ->
  ((< .. > as 'ls) -> 'v -> 's sess) ->
  'v ->
  ('r, 'ls) send sess ->
  's sess =
  fun _ sel v {once;prot=Send (_,ls)} ->
  Flag.use once;
  let s = sel ls v in
  s

let receive : 'r 'ls.
  'r ->
  ('r, 'ls) receive sess -> 'ls Lwt.t =
  fun _ {once;prot=s} ->
  Flag.use once;
  match s with
  | Receive(_, lss) ->
     Lwt.choose (lss ())
  | DummyReceive ->
     failwith "DummyReceive encountered"

let close {once;prot=Close} = Flag.use once

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

end
