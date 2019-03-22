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

let lens_get_ = Lens.lens_get_
let lens_get = Lens.lens_get
let lens_put_ = Lens.lens_put_
let lens_put = Lens.lens_put
                              
type 't slots_ = 't slots lazy_t

type empty = Empty
type 'a data = {data:'a}
type 'a lin = {__lindata:'a}

type ('pre, 'post, 'a) monad = {__run:'pre -> ('post * 'a) Lwt.t}
type 'f bind = {__call:'f}

let return a =
  {__run=fun pre -> Lwt.return (pre, {data=a})}

let return_lin a =
  {__run=fun pre -> Lwt.return (pre, {__lindata=a})}

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

let bind_lin :
      'pre 'mid 'post 'a 'b.
      ('pre, 'mid, 'a lin) monad ->
      ('a lin -> ('mid, 'post, 'b) monad) bind ->
      ('pre, 'post, 'b) monad = fun m f ->
  {__run=
     fun pre ->
     Lwt.bind (m.__run pre) @@ fun (mid,a) ->
     (f.__call a).__run mid
  }

let lift t =
  {__run=
     fun pre ->
     Lwt.map (fun x -> (pre, {data=x})) t
  }
     

let run m =
  Lwt.map (fun (_,a) -> a.data) @@ m.__run (Lazy.from_val Lens.Nil)

let get_lin l =
  {__run=
     fun pre ->
     Lwt.return (Lens.lens_put_ l pre Empty, Lens.lens_get_ l pre)
  }

let put_linval l v =
  {__run=
     fun pre ->
     Lwt.return (Lens.lens_put_ l pre {__lindata=v}, {data=()})
  }

let expand =
  {__run=
     fun pre ->
     Lwt.return (Lazy.from_val (Lens.Cons(Lazy.from_val Empty, pre)), {data=()})
  }

let shrink =
  {__run=
     fun (lazy (Lens.Cons(_, pre))) ->
     Lwt.return (pre, {data=()})
  }

module Op = struct
  let (>>=) = bind
  let (>>-) = bind_lin
end

module Syntax = struct
  type 'f bind_ = 'f bind = { __call : 'f; }
  type 'a lin_ = 'a lin = { __lindata : 'a; }
  type 'a data_ = 'a data = { data : 'a; }
  let bind = bind
  let bind_lin = bind_lin
  let return_lin = return_lin
  let get_lin = get_lin
  let put_linval = put_linval
end
