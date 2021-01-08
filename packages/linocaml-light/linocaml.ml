open Concur_shims

type 'a lin = {__lin:'a}[@@ocaml.unboxed]
type 'a data = {data:'a}[@@ocaml.unboxed]

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

let rec lens_get : type x y xs ys. (x,y,xs,ys) lens -> xs -> x = fun[@inline][@specialise] l xs ->
  match l,xs with
  | Zero,(`cons(hd,_)) -> hd
  | Succ l,(`cons(_,tl)) -> lens_get l tl
  | Other(get,_),xs -> get xs

let rec lens_put : type x y xs ys. (x,y,xs,ys) lens -> xs -> y -> ys = fun[@inline][@specialise] l xs b ->
  match l,xs with
  | Zero,(`cons(_,tl)) -> `cons(b,tl)
  | Succ l,(`cons(hd,tl)) -> `cons(hd,lens_put l tl b)
  | Other (_,put),xs -> put xs b

type ('pre, 'post, 'a) monad = {__m:('pre -> ('post * 'a) IO.io)}

type 'f bind = 'f

type all_empty = [`cons of unit * 'xs] as 'xs

let[@inline] return a =
  {__m=(fun[@inline] pre -> IO.return (pre, {data=a}))}

let[@inline] return_lin v =
  {__m=(fun[@inline] pre ->
     IO.return (pre, {__lin=v}))}

let[@inline] bind m f =
  {__m=(fun[@inline] pre ->
       IO.bind (m.__m pre) (fun[@inline] (mid, {data=a}) -> (f a).__m mid))}

let[@inline] bind_ m1 m2 =
  {__m=(fun[@inline] pre ->
     IO.bind (m1.__m pre) (fun[@inline] (mid, _) -> m2.__m mid))}

let bind_lin : 'pre 'mid 'a 'post 'b. ('pre, 'mid, 'a lin) monad
            -> ('a lin -> ('mid, 'post, 'b) monad) bind
            -> ('pre, 'post, 'b) monad = fun[@inline] m f ->
  {__m=(fun[@inline] pre ->
     IO.bind (m.__m pre) (fun[@inline] (mid, x) -> (f x).__m mid))}

let (@>) : 'p 'q 'pre 'post 'a.
           ('p, 'q, 'a) monad
           -> ('p, 'q, 'pre, 'post) lens
           -> ('pre, 'post, 'a) monad = fun[@inline] m l ->
  {__m=(fun[@inline] pre ->
       IO.bind (m.__m (lens_get l pre)) (fun[@inline] (q, a) -> IO.return (lens_put l pre q, a)))}

let[@inline] (<@) l m = m @> l

let get_lin : 'a 'pre 'post. ('a lin, unit, 'pre, 'post) lens -> ('pre,'post,'a lin) monad =
  fun[@inline] l ->
  {__m=(fun[@inline] pre ->
     IO.return (lens_put l pre (), lens_get l pre))}

let put_lin : 'a 'mid 'post 'pre. (unit,'a lin,'mid,'post) lens -> ('pre,'mid,'a lin) monad -> ('pre,'post,unit data) monad =
  fun[@inline] l m ->
  {__m=(fun[@inline] pre ->
     IO.bind (m.__m pre) (fun[@inline] (mid, v) ->
         IO.return (lens_put l mid v, {data=()})))}

let put_linval : 'a 'pre 'post. (unit,'a lin,'pre,'post) lens -> 'a -> ('pre,'post,unit data) monad =
  fun[@inline] l v ->
  {__m=(fun[@inline] pre ->
     IO.return (lens_put l pre {__lin=v}, {data=()}))}

let rec all_empty : all_empty = `cons((), all_empty)

let[@inline] thread_create l f x =
  {__m=(fun[@inline] lpre ->
       let ep = lens_get l lpre in
       let lpost = lens_put l lpre () in
       let th () =
         IO.bind ((f x).__m (`cons(ep,all_empty))) (fun[@inline] b ->
             let ((_:all_empty),{data=()}) = b in
             IO.return ())
       in
       let t = Thread.create th () in
       IO.return (lpost,{data=t}))}

let[@inline] lift m = {__m=(fun[@inline] p -> IO.bind m (fun[@inline] v -> IO.return (p, {data=v})))}

let[@inline] run f x =
  IO.bind ((f x).__m all_empty) (fun[@inline] (_, {data=x}) -> IO.return x)

let[@inline] run_ f =
  IO.bind ((f ()).__m all_empty) (fun[@inline] (_, {data=x}) -> IO.return x)

module Syntax = struct
  let bind_data = bind
  let bind_lin = bind_lin
  let[@inline] bind_raw {__m=m} f = {__m=(fun[@inline] pre -> IO.bind (m pre) (fun[@inline] (mid,x) -> (f x).__m mid))}
  let return_lin = return_lin
  let get_lin = get_lin
  let put_linval = put_linval
  let[@inline] lens_put' l v xs = lens_put l xs v

  module Internal = struct
    let[@inline] _lin x = {__lin=x}
    let[@inline] _unlin ({__lin=x}) = x
    let _mkbind : 'f. 'f -> 'f bind =
      fun[@inline] f -> f
    (* external _mkbind : 'a -> 'a = "%identity" *)
    let _run : 'pre 'post 'a. ('pre,'post,'a data) monad -> 'pre -> 'a IO.io =
      fun[@inline] m pre ->
         IO.bind (m.__m pre) (fun[@inline] (_, {data=v}) -> IO.return v)
    let[@inline] _dispose_env m =
      {__m=(fun[@inline] pre ->
         IO.bind (m.__m pre) (fun[@inline] (_,a) -> IO.return ((), a)))}
    let _peek : 'pre 'post 'a. ('pre -> ('pre, 'post, 'a) monad) -> ('pre, 'post, 'a) monad =
      fun[@inline] f ->
      {__m=(fun[@inline] pre -> ((f pre).__m pre))}
    let _poke : 'pre 'post. 'post -> ('pre, 'post, unit data) monad =
      fun[@inline] post ->
      {__m=(fun[@inline] _ -> IO.return (post, {data=()}))}
    let _map_lin : 'a 'b 'pre 'post. ('a -> 'b) -> ('a lin, 'b lin, 'pre, 'post) lens -> ('pre, 'post, unit data) monad
      = fun[@inline] f l ->
      {__m=fun[@inline] pre -> IO.return (lens_put l pre @@ _lin (f (_unlin @@ lens_get l pre)), {data=()})}
    let _modify : 'pre 'mid 'post 'a. ('pre -> 'mid)
            -> ('mid, 'post, 'a) monad
            -> ('pre, 'post, 'a) monad = fun f m ->
        {__m=(fun[@inline] pre -> m.__m (f pre))}
  end
end
