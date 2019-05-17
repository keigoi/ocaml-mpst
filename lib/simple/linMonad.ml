(* module Lens = Mpst_base.LensStrict
 * 
 * type 't slots = 't Lens.slots =
 *   Cons : 'x * 'xs slots -> ('x * 'xs) slots
 * | Nil : unit slots
 * and ('v1, 'v2, 's1, 's2) lens = ('v1,'v2,'s1,'s2) Lens.lens =
 *                                   Fst : ('a, 'b, ('a * 'xs) slots, ('b * 'xs) slots) lens
 *                               | Next :
 *                                   ('a, 'b, 'xs slots, 'ys slots) lens -> ('a, 'b, ('x * 'xs) slots,
 *                                                                           ('x * 'ys) slots)
 *                                                                            lens
 * 
 * let lens_get = Lens.lens_get
 * let lens_put = Lens.lens_put
 *                               
 * type empty = Empty
 * type 'a data = {data:'a}
 * type 'a lin = {__lindata:'a}
 * 
 * type ('pre, 'post, 'a) monad = {__run:'pre -> ('post * 'a)}
 * type 'f bind = {__call:'f}
 * 
 * let return a =
 *   {__run=fun pre -> (pre, {data=a})}
 * 
 * let return_lin a =
 *   {__run=fun pre -> (pre, {__lindata=a})}
 * 
 * let bind :
 *       'pre 'mid 'post 'a 'b.
 *       ('pre, 'mid, 'a data) monad ->
 *       ('a -> ('mid, 'post, 'b) monad) ->
 *       ('pre, 'post, 'b) monad = fun m f ->
 *   {__run=
 *      fun pre ->
 *      (m.__run pre) |> fun (mid,a) ->
 *      (f a.data).__run mid
 *   }
 * 
 * let bind_lin :
 *       'pre 'mid 'post 'a 'b.
 *       ('pre, 'mid, 'a lin) monad ->
 *       ('a lin -> ('mid, 'post, 'b) monad) bind ->
 *       ('pre, 'post, 'b) monad = fun m f ->
 *   {__run=
 *      fun pre ->
 *      (m.__run pre) |> fun (mid,a) ->
 *      (f.__call a).__run mid
 *   }
 * 
 * let run m =
 *   (snd (m.__run Lens.Nil)).data
 * 
 * let get_lin l =
 *   {__run=
 *      fun pre ->
 *      (Lens.lens_put l pre Empty, Lens.lens_get l pre)
 *   }
 * 
 * let put_linval l v =
 *   {__run=
 *      fun pre ->
 *      (Lens.lens_put l pre {__lindata=v}, {data=()})
 *   }
 * 
 * let expand =
 *   {__run=
 *      fun pre ->
 *      (Lens.Cons(Empty, pre), {data=()})
 *   }
 * 
 * let shrink =
 *   {__run=
 *      fun (Lens.Cons(_, pre)) ->
 *      (pre, {data=()})
 *   }
 * 
 * module Op = struct
 *   let (>>=) = bind
 *   let (>>-) = bind_lin
 * end
 * 
 * module Syntax = struct
 *   type 'f bind_ = 'f bind = { __call : 'f; }
 *   type 'a lin_ = 'a lin = { __lindata : 'a; }
 *   type 'a data_ = 'a data = { data : 'a; }
 *   let bind = bind
 *   let bind_lin = bind_lin
 *   let return_lin = return_lin
 *   let get_lin = get_lin
 *   let put_linval = put_linval
 * end *)
