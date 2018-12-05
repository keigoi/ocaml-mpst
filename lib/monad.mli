

type ('pre, 'post, 'a) monad

val return : 'a -> ('pre, 'pre, 'a) monad
val (>>=) :
  ('pre, 'mid, 'a) monad ->
  ('a -> ('mid, 'post, 'b) monad) ->
  ('pre, 'post, 'b) monad

val lift : 'a Lwt.t -> ('pre, 'pre, 'a) monad

val put : (_, 'v, 'pre, 'post) Base.lens -> 'v -> ('pre, 'post, unit) monad

val __in : ('pre -> ('post * 'a) Lwt.t) -> ('pre, 'post, 'a) monad
val __run : 'pre -> ('pre, 'post, 'a) monad -> ('post * 'a) Lwt.t
