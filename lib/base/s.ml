module type SESSION =
  sig
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
    module Session : SESSION
    open Session

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
      ('la, 'lb, 'sa sess, 'sb sess, 'v, 'v) label ->
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
    (* val choicemany_at :
     *   ('a, 'b, ('c, 'd) sendmany sess, 'e slots, 'f) role ->
     *   ('g, 'h, 'd) label_merge ->
     *   ('i, ('j, 'g) sendmany sess,
     *    close sess, 'k, 'e slots)
     *   role * 'k lazy_t ->
     *   ('l, ('c, 'h) sendmany sess,
     *    close sess, 'm, 'e slots)
     *   role * 'm lazy_t -> 'f lazy_t *)
    val loop : 'a lazy_t lazy_t -> 'a lazy_t
    val get_sess :
      ('a, 'b sess one, 'c, 'd, 'e) role -> 'd lazy_t -> 'b sess
    (* val add_conn : 'a -> X.conn -> 'b sess -> 'b sess
     * val add_conn_many :
     *   'a -> X.conn list -> 'b sess -> 'b sess *)
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

module type RAW =
  sig
    type conn
    val create : unit -> conn * conn
    val write : (Obj.t -> 'a) -> conn -> 'b -> unit
    val try_read : ('a -> Obj.t option) -> conn -> 'b Lwt.t
  end
    
