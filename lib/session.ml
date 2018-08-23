type _ select = Select__
type _ branch = Branch__
type _ accept = Accept__
type _ request = Request__
type _ disconnect = Disconnec__
type close = Close__

type ('r, 'k) conn = {mutable conn:'k option; origin:'r option}

type _ sess =
  | Select : 'a lazy_t -> 'a select sess
  | SelectMulti : 'a lazy_t -> 'a select sess
  | Branch : ('r1 * (unit -> 'a Lwt.t) list) -> ('r1 * 'a) branch sess (* "fancy" type rep. (hiding Lwt.t) *)
  | DummyBranch : 'a branch sess
  | Request : 'a -> 'a request sess
  | Accept : 'r1 * ('k1 -> 'a Lwt.t) list -> ('r1 * ('k1 -> 'a)) accept sess
  | Disconnect : 'a -> 'a disconnect sess
  | Close : close sess

let send : 'r 'l 'v 's. 'r -> ((< .. > as 'l) -> 'v -> 's sess) -> 'v -> ('r * 'l) select sess -> 's sess =
  fun _ g v (Select (lazy (_,r))|SelectMulti (lazy (_,r))) ->
  g r v

let request : 'r 'l 'v 's 'k. 'r -> ((< .. > as 'l) -> 'v -> 's sess) -> 'v -> 'k -> ('r * ('k -> 'l)) request sess -> 's sess =
  fun _ g v k (Request (_,r)) ->
  g (r k) v

let receive : 'r 'l. 'r -> ('r * 'l) branch sess -> 'l Lwt.t =
  fun _ s ->
  match s with
  | Branch (_,fs) ->
     Lwt.choose @@ List.map (fun f -> f ()) fs
  | DummyBranch -> Printf.eprintf"fail at receiption"; failwith "dummy branch encountered"

let accept : 'r 'l 'k. 'r -> 'k -> ('r * ('k -> 'l)) accept sess -> 'l Lwt.t =
  fun _ k (Accept (_, ls)) ->
  Lwt.choose (List.map (fun l -> l k) ls)


let disconnect : 'r 'k. 'r -> ('k -> unit) -> ('r * (_, 'k) conn * 's sess) disconnect sess -> 's sess =
  fun _ f (Disconnect (_, kr, s)) -> (match kr.conn with Some k -> f k; kr.conn <- None | None -> Printf.eprintf "fail at disconnection"; failwith "connection already closed"); s

let close : close sess -> unit = fun _ -> ()
