type _ select = Select__
type _ branch = Branch__
type _ selectbranch = SelectBranch__
type _ accept = Accept__
type _ request = Request__
type _ disconnect = Disconnec__
type close = Close__

type _ sess =
  Select : 'a lazy_t -> 'a select sess
| Branch : ('r1 * (unit -> 'a Lwt.t) list) -> ('r1 * 'a) branch sess
| DummyBranch : 'a branch sess
| SelectBranch : 'a lazy_t -> 'a selectbranch sess
| Request : 'a -> 'a request sess
| Accept : 'r1 *
             ('k1 -> 'a Lwt.t) list -> ('r1 * ('k1 -> 'a)) accept sess
| Disconnect : 'a -> 'a disconnect sess
| Close : close sess

type ('r, 'k) conn = { mutable conn : 'k option; origin : 'r option; }

type 't proc
val mkproc : (unit -> 't Lwt.t) -> 't proc

val send :
  'r ->
  ((< .. > as 'a) -> 'v -> 's sess) ->
  'v -> ('r * 'a) select sess -> 's sess

val receive : 'r -> ('r * 'l) branch sess -> 'l Lwt.t

val accept : 'r -> 'k -> ('r * ('k -> 'l)) accept sess -> 'l Lwt.t

val request :
  'r ->
  ((< .. > as 'a) -> 'v -> 's sess) ->
  'v -> 'k -> ('r * ('k -> 'a)) request sess -> 's sess

val disconnect :
  'r ->
  ('k -> unit Lwt.t) -> ('r * ('a, 'k) conn * 's sess) disconnect sess -> 's sess Lwt.t

val close : close sess -> unit

val send_receive : 'r1 * 'r2 -> ((< .. > as 'l1) -> 'v -> 'l2 proc) -> 'v -> (('r1 * 'r2) * 'l1) selectbranch sess -> 'l2 Lwt.t

val _receive : ('r * 'l) branch sess -> 'l Lwt.t
