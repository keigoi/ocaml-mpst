module type COMM_LIN = sig

    (** {2 Bare Channel Types} *)

    type ('v, 's) out
    (** Output of type 'v, then continue to session 's *)
  
    type 'var inp
    (** Input of type 'var *)
  
    type close
    (** Termination of a session *)

    type ('v, 's) out_many

    type 'var inp_many

    (** {2 Preamble} *)

    type ('pre,'post,'a) monad = ('pre,'post,'a) Linocaml.monad

    type ('a,'b,'c,'d) lens = ('a,'b,'c,'d) Linocaml.lens

    type 'a lin = 'a Linocaml.lin

    type 'a data = 'a Linocaml.data
    
    type all_empty = Linocaml.all_empty

    (** {2 Primitives} *)

    val send :
        ('s lin, unit, 'pre, 'post) lens ->
        ((< .. > as 's) -> ('v data, 't) out) -> 'v ->
        ('pre, 'post, 't lin) monad
    (** Output a value *)

    val deleg_send :
        ('s lin, unit, 'mid, 'post) lens ->
        ((< .. > as 's) -> ('t lin, 'u) out) ->
        ('t lin, unit, 'pre, 'mid) lens ->
        ('pre, 'post, 'u lin) monad
    (** Delegation of a session *)

    val receive :
        ('s lin, unit, 'pre, 'post) lens ->
        ((< .. > as 's) -> 'var inp) ->
        ('pre, 'post, 'var lin) monad
    (** Input a value *)

    val send_many :
        ('a lin, unit, 'b, 'c) lens ->
        ((< .. > as 'a) -> ('d data, 'e) out_many) -> (int -> 'd) -> ('b, 'c, 'e lin) monad

    val receive_many :
        ('a lin, unit, 'b, 'c) lens -> 
        ('a -> 'd inp_many) -> ('b, 'c, 'd lin) monad

    val close :
        (close lin, unit, 'pre, 'post) lens ->
        ('pre, 'post, unit data) monad
    (** Terminate the session *)

    val create_thread_lin :
        ('s lin, unit, 'pre, 'post) lens ->
        (unit, 's lin, all_empty, 'pre2) lens ->
        (unit -> ('pre2, all_empty, unit data) monad) ->
        ('pre, 'post, unit data) monad


    (** {1 Linocaml-Style Communication Primitives} *)

    module LinocamlStyle : sig
        val s0 : ('x, 'y, 'x, 'y) lens

        val ( @* ) :
        ('a,'b,'q,'r) lens
        -> ('c,unit,'p,'q) lens
        -> ('a * 'c,'b,'p,'r) lens

        val send : ((< .. > as 's) -> ('v data, 't) out) -> 'v -> ('s lin, unit, 't lin) monad

        val deleg_send : ((< .. > as 's) -> ('t lin, 'u) out) -> ('s lin * 't lin, unit, 'u lin) monad

        val receive : ((< .. > as 's) -> 'var inp) -> ('s lin, unit, 'var lin) monad

        val close : (close lin, unit, unit data) monad

        val create_thread_lin :
        (unit -> ('a lin, unit, unit data) monad) -> ('a lin, unit, unit data) monad
    end
end

module type GEN_LIN = sig
    
    type kind = [ `IPCProcess | `Local | `Untyped ]

    type 'a tup

    type 't global

    type close

    type ('pre,'post,'a) monad = ('pre,'post,'a) Linocaml.monad

    type ('a,'b,'c,'d) lens = ('a,'b,'c,'d) Linocaml.lens

    type 'a lin = 'a Linocaml.lin

    type 'a data = 'a Linocaml.data
    
    type all_empty = Linocaml.all_empty

    type 'a one = 'a Mpst.Types.one

    type ('a, 'b, 'c, 'd, 'e, 'f) role =  ('a, 'b, 'c, 'd, 'e, 'f) Mpst.Types.role


  (** {1 Extracting Channel Vectors From Global Combinators (Linearly-Typed)} *)

    val gen : 'a global -> ('b, 'b, 'a tup lin) monad

    val gen_mult : int list -> 'a global -> ('b, 'b, 'a tup lin) monad

    val gen_with_kinds :
        kind list ->
        'a global -> ('b, 'b, 'a tup lin) monad

    val gen_with_kinds_mult :
        (kind * int) list ->
        'a global -> ('b, 'b, 'a tup lin) monad

    val degen :
        (([ `cons of close one * 'a ] as 'a) tup lin, unit, unit data)
        monad

    val get_ch :
        ('a one, close one, 'b, 'c, 'd, 'e) role ->
        ('b tup lin, unit, ('c tup lin * 'a lin) lin) monad

    val get_ch_list :
        ('a list, close one, 'b, 'c, 'd, 'e) role ->
        ('b tup lin, unit, ('c tup lin * 'a list lin) lin) monad

    val raw_gen : 'a global -> 'a tup

    val raw_gen_mult : int list -> 'a global -> 'a tup

    val raw_gen_with_kinds :
        kind list -> 'a global -> 'a tup

    val raw_gen_with_kinds_mult :
        (kind * int) list -> 'a global -> 'a tup

    val raw_get_ch : ('a one, 'b, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a
end

module type PORTS_LIN = sig
    type kind = [ `IPCProcess | `Local | `Untyped ]

    type 't shared

    type 't global

    type ('pre,'post,'a) monad = ('pre,'post,'a) Linocaml.monad
    type 'a lin = 'a Linocaml.lin
    type 'a one = 'a Mpst.Types.one
    type ('a, 'b, 'c, 'd, 'e, 'f) role =  ('a, 'b, 'c, 'd, 'e, 'f) Mpst.Types.role

    val create_shared :
      ?kinds:(kind * int) list ->
      [ `cons of 'a * 'b ] global -> [ `cons of 'a * 'b ] shared

      (* 
    val accept_ :
      [ `cons of 'a * 'b ] shared ->
      ('c one, 'd, [ `cons of 'a * 'b ], 'e, 'f, 'g) role ->
      'c Concur_shims.IO.io

    val connect_ :
      [ `cons of 'a * 'b ] shared ->
      ('c one, 'd, [ `cons of 'a * 'b ], 'e, 'f, 'g) role ->
      'c Concur_shims.IO.io *)

    val accept :
      [ `cons of 'a * 'b ] shared ->
      ('c one, 'd, [ `cons of 'a * 'b ], 'e, 'f, 'g) role ->
      ('h, 'h, 'c lin) Linocaml.monad

    val connect :
      [ `cons of 'a * 'b ] shared ->
      ('c one, 'd, [ `cons of 'a * 'b ], 'e, 'f, 'g) role ->
      ('h, 'h, 'c lin) Linocaml.monad
end
