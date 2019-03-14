module Lens = Lens.Make(struct type 't u = 't end)

type 't slots = 't Lens.slots =
  Cons : 'x lazy_t * 'xs slots lazy_t -> ('x * 'xs) slots
| Nil : unit slots
and ('v1, 'v2, 's1, 's2) lens = ('v1,'v2,'s1,'s2) Lens.lens =
                                  Fst : ('a, 'b, ('a * 'xs) slots, ('b * 'xs) slots) lens
                              | Next :
                                  ('a, 'b, 'xs slots, 'ys slots) lens -> ('a, 'b, ('x * 'xs) slots,
                                                                          ('x * 'ys) slots)
                                                                           lens
type 't slots_ = 't slots lazy_t
               
type 'a data = {data:'a}
type 'a lin = {__lindata:'a}

type ('pre, 'post, 'a) monad = {__run:'pre -> ('post * 'a) Lwt.t}
type 'f bind = {__call:'f}

let return a =
  {__run=fun pre -> Lwt.return (pre, {data=a})}
  
let bind :
      'pre 'mid 'post 'a 'b.
      ('pre, 'mid, 'a data) monad ->
      ('a -> ('mid, 'post, 'b) monad) ->
      ('pre, 'post, 'b) monad = fun m f ->
  {__run=
     fun pre ->
     Lwt.bind (m.__run pre) @@ fun (mid,a) ->
                               (f a.data).__run mid
  }
  
let linbind :
      'pre 'mid 'post 'a 'b.
      ('pre, 'mid, 'a lin) monad ->
      ('a lin -> ('mid, 'post, 'b) monad) bind ->
      ('pre, 'post, 'b) monad = fun m f ->
  {__run=
     fun pre ->
     Lwt.bind (m.__run pre) @@ fun (mid,a) ->
                               (f.__call a).__run mid
  }

let run m =
  Lwt.map (fun (_,a) -> a.data) @@ m.__run (Lazy.from_val Lens.Nil)

let at m l =
  {__run=
     fun pre ->
     Lwt.bind (m.__run (Lens.lens_get_ l pre)) @@ fun (q, a) ->
                                                  Lwt.return (Lens.lens_put_ l pre q, a)
  }

let expand =
  {__run=
     fun pre ->
     Lwt.return (Lazy.from_val (Lens.Cons(Lazy.from_val (), pre)), ())
  }

let shrink =
  {__run=
     fun (lazy (Lens.Cons(_, pre))) ->
     Lwt.return (pre, ())
  }

module Op = struct
  let (>>=) = bind
  let (>>-) = linbind
  let (@>) = at
end
