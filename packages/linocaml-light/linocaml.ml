open Concur_shims

type 'a lin = {__lin:'a}
type 'a data = {data:'a}

type (_,_,_,_) lens =
  | Zero : ('a,'b,[`cons of 'a * 'xs], [`cons of 'b * 'xs]) lens
  | Succ :
      ('x,'y,'xs,'ys) lens
      -> ('x,'y,[`cons of 'a * 'xs], [`cons of 'a * 'ys]) lens
  | Other :
      ('xs -> 'x) * ('xs -> 'y -> 'ys)
      -> ('x,'y,'xs,'ys) lens

let _0 = Zero

let _1 = Succ Zero

let _2 = Succ (Succ Zero)

let rec lens_get : type x y xs ys. (x,y,xs,ys) lens -> xs -> x = fun l xs ->
  match l,xs with
  | Zero,(`cons(hd,_)) -> hd
  | Succ l,(`cons(_,tl)) -> lens_get l tl
  | Other(get,_),xs -> get xs

let rec lens_put : type x y xs ys. (x,y,xs,ys) lens -> xs -> y -> ys = fun l xs b ->
  match l,xs with
  | Zero,(`cons(_,tl)) -> `cons(b,tl)
  | Succ l,(`cons(hd,tl)) -> `cons(hd,lens_put l tl b)
  | Other (_,put),xs -> put xs b

type ('pre, 'post, 'a) monad = {__m:('pre -> ('post * 'a) IO.io)}

type 'f bind = 'f

type all_empty = [`cons of unit * 'xs] as 'xs

let return a =
  {__m=(fun pre -> IO.return (pre, {data=a}))}

let return_lin v =
  {__m=(fun pre ->
     IO.return (pre, {__lin=v}))}

let bind m f =
  {__m=(fun pre ->
       IO.bind (m.__m pre) (fun (mid, {data=a}) -> (f a).__m mid))}

let bind_ m1 m2 =
  {__m=(fun pre ->
     IO.bind (m1.__m pre) (fun (mid, _) -> m2.__m mid))}

let bind_lin : 'pre 'mid 'a 'post 'b. ('pre, 'mid, 'a lin) monad
            -> ('a lin -> ('mid, 'post, 'b) monad) bind
            -> ('pre, 'post, 'b) monad = fun m f ->
  {__m=(fun pre ->
     IO.bind (m.__m pre) (fun (mid, x) -> (f x).__m mid))}

let (@>) : 'p 'q 'pre 'post 'a.
           ('p, 'q, 'a) monad
           -> ('p, 'q, 'pre, 'post) lens
           -> ('pre, 'post, 'a) monad = fun m l ->
  {__m=(fun pre ->
       IO.bind (m.__m (lens_get l pre)) (fun (q, a) -> IO.return (lens_put l pre q, a)))}

let (<@) l m = m @> l

let get_lin : 'a 'pre 'post. ('a lin, unit, 'pre, 'post) lens -> ('pre,'post,'a lin) monad =
  fun l ->
  {__m=(fun pre ->
     IO.return (lens_put l pre (), lens_get l pre))}

let put_lin : 'a 'mid 'post 'pre. (unit,'a lin,'mid,'post) lens -> ('pre,'mid,'a lin) monad -> ('pre,'post,unit data) monad =
  fun l m ->
  {__m=(fun pre ->
     IO.bind (m.__m pre) (fun (mid, v) ->
         IO.return (lens_put l mid v, {data=()})))}

let put_linval : 'a 'pre 'post. (unit,'a lin,'pre,'post) lens -> 'a -> ('pre,'post,unit data) monad =
  fun l v ->
  {__m=(fun pre ->
     IO.return (lens_put l pre {__lin=v}, {data=()}))}

let run =
  fun f x ->
  let rec all_empty = `cons((), all_empty) in
  IO.bind ((f x).__m all_empty) (fun (_, {data=x}) -> IO.return x)

let run_ =
  fun f ->
  let rec all_empty = `cons((), all_empty) in
  IO.bind ((f ()).__m all_empty) (fun (_, {data=x}) -> IO.return x)

module Syntax = struct
  let bind_data = bind
  let bind_lin = bind_lin
  let bind_raw {__m=m} f = {__m=(fun pre -> IO.bind (m pre) (fun (mid,x) -> (f x).__m mid))}
  let return_lin = return_lin
  let get_lin = get_lin
  let put_linval = put_linval

  module Internal = struct
    let _lin x = {__lin=x}
    let _unlin ({__lin=x}) = x
    let _mkbind : 'f. 'f -> 'f bind =
      fun f -> f
    let _run : 'pre 'post 'a. ('pre,'post,'a data) monad -> 'pre -> 'a IO.io =
      fun m pre ->
         IO.bind (m.__m pre) (fun (_, {data=v}) -> IO.return v)
    let _dispose_env m =
      {__m=(fun pre ->
         IO.bind (m.__m pre) (fun (_,a) -> IO.return ((), a)))}
    let _peek : 'pre 'post 'a. ('pre -> ('pre, 'post, 'a) monad) -> ('pre, 'post, 'a) monad =
      fun f ->
      {__m=(fun pre -> ((f pre).__m pre))}
    let _poke : 'pre 'post. 'post -> ('pre, 'post, unit data) monad =
      fun post ->
      {__m=(fun _ -> IO.return (post, {data=()}))}
    let _map_lin : 'a 'b 'pre 'post. ('a -> 'b) -> ('a lin, 'b lin, 'pre, 'post) lens -> ('pre, 'post, unit data) monad
      = fun f l ->
      {__m=fun pre -> IO.return (lens_put l pre @@ _lin (f (_unlin @@ lens_get l pre)), {data=()})}
  end
end
