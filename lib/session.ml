open Base

type _ select = Select__
type _ branch = Branch__
type _ selectbranch = SelectBranch__
type _ accept = Accept__
type _ request = Request__
type _ disconnect = Disconnec__
type close = Close__

type ('r, 'k) conn = {mutable conn:'k option; origin:'r option}
type 't proc = unit -> 't Lwt.t

let mkproc t = t

type _ sess =
  | Select : 'a lazy_t -> 'a select sess
  | Branch : ('r1 * (unit -> 'a Lwt.t) list) -> ('r1 * 'a) branch sess (* "fancy" type rep. (hiding Lwt.t) *)
  | DummyBranch : 'a branch sess
  | SelectBranch : 'a lazy_t -> 'a selectbranch sess
  | Request : 'a -> 'a request sess
  | Accept : 'r1 * ('k1 -> 'a Lwt.t) list -> ('r1 * ('k1 -> 'a)) accept sess
  | Disconnect : 'a -> 'a disconnect sess
  | Close : close sess

let send : 'r 'l 'v 's. 'r -> ((< .. > as 'l) -> 'v -> 's sess) -> 'v -> ('r * 'l) select sess -> 's sess =
  fun _ g v (Select (lazy (_,r))) ->
  g r v

let request : 'r 'l 'v 's 'k. 'r -> ((< .. > as 'l) -> 'v -> 's sess) -> 'v -> 'k -> ('r * ('k -> 'l)) request sess -> 's sess =
  fun _ g v k (Request (_,r)) ->
  g (r k) v

let _receive : 'r 'l. ('r * 'l) branch sess -> 'l Lwt.t =
  fun s ->
  match s with
  | Branch (_,fs) -> Lwt.choose @@ List.map (fun f -> f ()) fs
  | DummyBranch -> Printf.eprintf"fail at receiption"; failwith "dummy branch encountered"

let receive : 'r 'l. 'r -> ('r * 'l) branch sess -> 'l Lwt.t =
  fun _ -> _receive

let accept : 'r 'l 'k. 'r -> 'k -> ('r * ('k -> 'l)) accept sess -> 'l Lwt.t =
  fun _ k (Accept (_, ls)) ->
  Lwt.choose (List.map (fun l -> l k) ls)

let disconnect : 'r 'k. 'r -> ('k -> unit Lwt.t) -> ('r * (_, 'k) conn * 's sess) disconnect sess -> 's sess Lwt.t =
  fun _ f (Disconnect (_, kr, s)) ->
  match kr.conn with
  | Some k -> Lwt.bind (f k) @@ fun () -> kr.conn <- None; Lwt.return s
  | None -> Printf.eprintf "fail at disconnection"; failwith "connection already closed"

let close : close sess -> unit = fun _ -> ()

let send_receive : 'r1 * 'r2 -> ((< .. > as 'l1) -> 'v -> 'l2 proc) -> 'v -> (('r1 * 'r2) * 'l1) selectbranch sess -> 'l2 Lwt.t =
  fun _ g v (SelectBranch (lazy (_, r))) ->
  g r v ()
