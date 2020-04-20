(* A lightweight version of LinOCaml https://github.com/keigoi/linocaml/ *)

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

type ('pre, 'post, 'a) monad = {__m:('pre -> ('post * 'a) Concur_shims.IO.io)}

type all_empty = [`cons of unit * 'xs] as 'xs

type 'f bind

val lens_get : ('x,'y,'xs,'ys) lens -> 'xs -> 'x

val lens_put : ('x,'y,'xs,'ys) lens -> 'xs -> 'y -> 'ys

val _0 : ('a, 'b, [`cons of 'a * 'xs], [`cons of 'b * 'xs]) lens

val _1 : ('a, 'b, [`cons of 'x1 * [`cons of 'a * 'xs]], [`cons of 'x1 * [`cons of 'b * 'xs]]) lens

val _2 : ('a, 'b, [`cons of 'x1 * [`cons of 'x2 * [`cons of 'a * 'xs]]], [`cons of 'x1 * [`cons of 'x2 * [`cons of 'b * 'xs]]]) lens

val return : 'a -> ('pre, 'pre, 'a data) monad

val bind : ('pre, 'mid, 'a data) monad
            -> ('a -> ('mid, 'post, 'b) monad)
            -> ('pre, 'post, 'b) monad

val bind_ : ('pre, 'mid, 'a data) monad
           -> ('mid, 'post, 'b) monad
           -> ('pre, 'post, 'b) monad

val bind_lin : ('pre, 'mid, 'a lin) monad
            -> ('a lin -> ('mid, 'post, 'b) monad) bind
            -> ('pre, 'post, 'b) monad

val return_lin : 'a -> ('p,'p,'a lin) monad

val (@>) : ('p, 'q, 'a) monad
           -> ('p, 'q, 'pre, 'post) lens
           -> ('pre, 'post, 'a) monad

val (<@) : ('p, 'q, 'pre, 'post) lens
          -> ('p, 'q, 'a) monad
          -> ('pre, 'post, 'a) monad

val get_lin : ('a lin, unit, 'pre, 'post) lens -> ('pre,'post,'a lin) monad

val put_lin : (unit,'a lin,'mid,'post) lens -> ('pre,'mid,'a lin) monad -> ('pre,'post,unit data) monad

val put_linval : (unit,'a lin,'pre,'post) lens -> 'a -> ('pre,'post,unit data) monad

val thread_create :
  ('a, unit, 'b, 'c) lens ->
  ('d -> ([> `cons of 'a * all_empty ], all_empty, unit data) monad) ->
  'd -> ('b, 'c, Concur_shims.Thread.t data) monad

val lift : 'a Concur_shims.IO.io -> ('p, 'p, 'a data) monad 

val run : ('a -> (all_empty, all_empty, 'b data) monad) -> 'a -> 'b Concur_shims.IO.io

val run_ : (unit -> (all_empty, all_empty, 'b data) monad) -> 'b Concur_shims.IO.io



module Syntax : sig
  val bind_data : ('pre,'mid,'a data) monad -> ('a -> ('mid,'post,'b) monad) -> ('pre,'post,'b) monad
  val bind_lin : ('pre,'mid,'a lin) monad -> ('a lin -> ('mid,'post,'b) monad) bind -> ('pre,'post,'b) monad
  val bind_raw : ('pre,'mid,'a) monad -> ('a -> ('mid,'post,'b) monad) bind -> ('pre,'post,'b) monad
  val return_lin : 'a -> ('p,'p,'a lin) monad
  val get_lin : ('a lin, unit, 'pre, 'post) lens -> ('pre,'post,'a lin) monad
  val put_linval : (unit,'a lin,'pre,'post) lens -> 'a -> ('pre,'post,unit data) monad

  module Internal : sig
    val _mkbind : 'f -> 'f bind
    val _run : ('pre,'post,'a data) monad -> 'pre -> 'a Concur_shims.IO.io
    val _dispose_env : ('pre,'all_empty,'a) monad -> ('pre,unit,'a) monad
    val _peek : ('pre -> ('pre, 'post, 'a) monad) -> ('pre, 'post, 'a) monad
    val _poke : 'post -> ('pre, 'post, unit data) monad
    val _map_lin : ('a -> 'b) -> ('a lin, 'b lin, 'pre, 'post) lens -> ('pre, 'post, unit data) monad
  end
end
