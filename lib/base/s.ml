module type FLAG = sig
  type t
  val create     : unit -> t
  val use        : t -> unit
  val try_use    : t -> bool
end

module type SESSION =
  sig
    module Flag : FLAG
    type ('r, 'ls) send
    type ('r, 'ls) receive
    type close
    (* type ('r, 'ls) sendmany *)
    (* type ('r, 'ls) receivemany *)
    type _ prot
    type 'p sess
    val send :
      ([>  ] as 'a) ->
      ((< .. > as 'b) -> 'v -> 's sess) ->
      'v -> ('a, 'b) send sess -> 's sess
    val receive : ([>  ] as 'a) -> ('a, 'ls) receive sess -> 'ls Lwt.t
    val close : close sess -> unit
    (* val scatter :
     *   ([>  ] as 'a) ->
     *   ((< .. > as 'b) -> 'v -> 's sess) ->
     *   (int -> 'v) -> ('a, 'b) sendmany sess -> 's sess
     * val gather : ([>  ] as 'a) -> ('a, 'ls) receivemany sess -> 'ls Lwt.t *)
    module Internal : sig val merge : 't prot -> 't prot -> 't prot end
  end

module type GLOBAL =
  sig
    module Sess : SESSION
    open Sess

    type _ lin
    type _ e
    type _ one
    type _ slots =
      Cons : 'x e lazy_t * 'xs slots lazy_t -> ('x * 'xs) slots
    | Nil : unit slots
    and (_, _, _, _) lens =
      Fst : ('a, 'b, ('a * 'xs) slots, ('b * 'xs) slots) lens
    | Next :
        ('a, 'b, 'xs slots, 'ys slots) lens -> ('a, 'b, ('x * 'xs) slots,
                                                ('x * 'ys) slots)
                                                 lens
    val lens_get : ('a, 'b, 'xs, 'ys) lens -> 'xs lazy_t -> 'a e lazy_t
    val lens_get_ : ('a, 'b, 'c, 'd) lens -> 'c lazy_t -> 'a e
    val lens_put :
      ('a, 'b, 'xs, 'ys) lens -> 'xs lazy_t -> 'b e lazy_t -> 'ys lazy_t
    val lens_put_ : ('a, 'b, 'c, 'd) lens -> 'c lazy_t -> 'b e -> 'd lazy_t
    type ('la, 'lb, 'ca, 'cb, 'v1, 'v2) label
    type ('r, 'v1, 'v2, 's1, 's2) role = {
        role : 'r;
        lens : ('v1, 'v2, 's1, 's2) lens;
      }
    val ( --> ) :
      ('ra, 'sa sess one, ('rb, 'la) send sess one, 'c0, 'c1)
        role ->
      ('rb, 'sb sess one, ('ra, 'lb) receive sess one, 'c1,
       'c2)
        role ->
      ('la, 'lb, 'sa sess lin, 'sb sess lin, 'v, 'v) label ->
      'c0 lazy_t -> 'c2 lazy_t
    (* val ( -->> ) :
     *   ('ra, 'sa sess, ('rb, 'la) sendmany sess, 'c1,
     *    'c2)
     *   role ->
     *   ('rb, 'sb sess, ('ra, 'lb) receive sess, 'c0,
     *    'c1)
     *   role ->
     *   ('la, 'lb, 'sa sess, 'sb sess, 'v, 'v) label ->
     *   'c0 lazy_t -> 'c2 lazy_t
     * val ( >>-- ) :
     *   ('ra, 'sa sess, ('rb, 'la) send sess, 'c0, 'c1)
     *   role ->
     *   ('rb, 'sb sess, ('ra, 'lb) receivemany sess,
     *    'c1, 'c2)
     *   role ->
     *   ('la, 'lb, 'sa sess, 'sb sess, 'v, 'v list) label ->
     *   'c0 lazy_t -> 'c2 lazy_t *)
    val dummy_receive :
      ('a, 'b, ('c, 'd) receive sess one, 'e, 'f) role ->
      'e lazy_t -> 'f lazy_t
    val dummy_close :
      ('a, 'b, close sess one, 'c, 'd) role ->
      'c lazy_t -> 'd lazy_t
    type ('l, 'r, 'lr) label_merge = { label_merge : 'l -> 'r -> 'lr; }
    val merge_ : 't slots lazy_t -> 't slots lazy_t -> 't slots lazy_t
    val choice_at :
      ('a, 'b, ('c, 'd) send sess one, 'e slots, 'f) role ->
      ('g, 'h, 'd) label_merge ->
      ('i, ('j, 'g) send sess one, close sess one,
       'k, 'e slots)
        role * 'k lazy_t ->
      ('l, ('c, 'h) send sess one, close sess one,
       'm, 'e slots)
        role * 'm lazy_t -> 'f lazy_t
    val loop : 'a lazy_t lazy_t -> 'a lazy_t
    val get_sess :
      ('a, 'b sess one, 'c, 'd, 'e) role -> 'd lazy_t -> 'b sess
    val nil : unit slots lazy_t
    val one :
      'a slots lazy_t -> (close sess one * 'a) slots lazy_t
  end

module type UTIL =
  sig
    module Global : GLOBAL
    val msg :
      (< msg : 'a -> 'b >, [> `msg of 'c * 'd ], 'b, 'd, 'a, 'c) Global.label
    val left :
      (< left : 'a -> 'b >, [> `left of 'c * 'd ], 'b, 'd, 'a, 'c) Global.label
    val right :
      (< right : 'a -> 'b >, [> `right of 'c * 'd ], 'b, 'd, 'a, 'c)
        Global.label
    val left_or_right :
      (< left : 'a; .. >, < right : 'b; .. >, < left : 'a; right : 'b >)
        Global.label_merge
    val right_or_left :
      (< right : 'a; .. >, < left : 'b; .. >, < left : 'b; right : 'a >)
        Global.label_merge
  end

module type MPST = sig
  module Session : SESSION
  module Global : GLOBAL with module Sess = Session
  module Util : UTIL with module Global = Global
end

module type RAW =
  sig
    type conn
    val create : unit -> conn * conn
    val write : (Obj.t -> 'a) -> conn -> 'b -> unit
    val try_read : ('a -> Obj.t option) -> conn -> 'b Lwt.t
  end

module type LIN = sig
  type 'a lin
  val mklin : 'a -> 'a lin
end
                 
module type LIN_MONAD = sig
  type _ slots =
    Cons : 'x lazy_t * 'xs slots lazy_t -> ('x * 'xs) slots
  | Nil : unit slots
  and (_, _, _, _) lens =
    Fst : ('a, 'b, ('a * 'xs) slots, ('b * 'xs) slots) lens
  | Next :
      ('a, 'b, 'xs slots, 'ys slots) lens -> ('a, 'b, ('x * 'xs) slots,
                                              ('x * 'ys) slots)
                                               lens
  val lens_get : ('a, 'b, 'xs, 'ys) lens -> 'xs lazy_t -> 'a lazy_t
  val lens_get_ : ('a, 'b, 'c, 'd) lens -> 'c lazy_t -> 'a
  val lens_put :
    ('a, 'b, 'xs, 'ys) lens -> 'xs lazy_t -> 'b lazy_t -> 'ys lazy_t
  val lens_put_ : ('a, 'b, 'c, 'd) lens -> 'c lazy_t -> 'b -> 'd lazy_t

  type 't slots_ = 't slots lazy_t

  type empty = Empty
  type 'a data = { data : 'a; }
  type 'a lin = { __lindata : 'a; }
  type 'f bind = { __call : 'f; }
  type ('pre, 'post, 'a) monad = {__run:'pre -> ('post * 'a) Lwt.t}

  val return : 'a -> ('b, 'b, 'a data) monad
  val bind :
    ('pre, 'mid, 'a data) monad ->
    ('a -> ('mid, 'post, 'b) monad) -> ('pre, 'post, 'b) monad
  val bind_lin :
    ('pre, 'mid, 'a lin) monad ->
    ('a lin -> ('mid, 'post, 'b) monad) bind -> ('pre, 'post, 'b) monad
  val run :
    (unit slots_, unit slots_, 'a data) monad ->
    'a Lwt.t
  val lift :
    'a Lwt.t -> ('pre, 'pre, 'a data) monad
  val expand :
    ('t slots lazy_t, (empty * 't) slots lazy_t, unit data) monad
  val shrink :
    ((empty * 't) slots lazy_t, 't slots lazy_t, unit data) monad

  module Op :
  sig
    val ( >>= ) :
      ('pre, 'mid, 'a data) monad ->
      ('a -> ('mid, 'post, 'b) monad) ->
      ('pre, 'post, 'b) monad
    val ( >>- ) :
      ('pre, 'mid, 'a lin) monad ->
      ('a lin -> ('mid, 'post, 'b) monad) bind ->
      ('pre, 'post, 'b) monad
  end

  module Syntax :
  sig
    type 'f bind_ = 'f bind = { __call : 'f; }
    type 'a lin_ = 'a lin = { __lindata : 'a; }
    type 'a data_ = 'a data = { data : 'a; }
    val bind :
      ('pre, 'mid, 'a data) monad ->
      ('a -> ('mid, 'post, 'b) monad) -> ('pre, 'post, 'b) monad
    val bind_lin :
      ('pre, 'mid, 'a lin) monad ->
      ('a lin -> ('mid, 'post, 'b) monad) bind -> ('pre, 'post, 'b) monad
    val return_lin : 'a -> ('b, 'b, 'a lin) monad
    val get_lin : ('a lin, empty, 'pre, 'post) lens -> ('pre lazy_t,'post lazy_t,'a lin) monad
    val put_linval : (empty,'a lin,'pre,'post) lens -> 'a -> ('pre lazy_t,'post lazy_t,unit data) monad
  end


end
