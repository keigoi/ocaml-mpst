open Concur_shims

module type COMM = sig

  (** {2 Bare Channel Types} *)

  type ('v, 's) out
  (** Output of type 'v, then continue to session 's *)

  type 'var inp
  (** Input of type 'var *)

  type close
  (** Termination of a session *)

  type ('v, 's) out_many
  (** Output values of type 'v to multiple recipients *)

  type 'var inp_many
  (** Input values from multiple senders *)

  (** {2 Primitives} *)

  val send : ('v, 't) out -> 'v -> 't IO.io
  (** Output a value *)

  val receive : 'var inp -> 'var IO.io
  (** Input a value *)

  val close : close -> unit IO.io
  (** Close the channel *)

  val send_many : ('v, 't) out_many -> (int -> 'v) -> 't IO.io
  
  val receive_many : 'var inp_many -> 'var IO.io
end  

module type GLOBAL_COMBINATORS = sig

  (** *)

  (** {2 Bare Channel Types (erased, ignore this)} *)

  type ('v,'t) out
  type 'var inp
  type ('v,'t) out_many
  type 'var inp_many
  type close

  (** {2 Common types} *)
     
  type 't global
  (** Type of a global protocol specification, where 't is of form [`cons of 't1 * [`cons of 't2 * ...]] *)

  type 'a lin
  (** Linear type constructor, which is expanded to 'a when dynamic linearity checking  *)

  type ('obj,'ot,'var,'vt) label = ('obj,'ot,'var,'vt) Types.label 

  type ('ts, 't, 'us, 'u, 'robj, 'mt) role = ('ts, 't, 'us, 'u, 'robj, 'mt) Types.role

  type ('lr, 'l, 'r) disj = ('lr, 'l, 'r) Types.disj

  type 'a one = 'a Types.one

  (** {2 Combinators} *)

  val ( --> ) :
    ('a one, 'b one, 'c, 'd, 'e, 'f inp) role ->
    ('g one, 'e one, 'h, 'c, 'b, 'i) role ->
    ('i, ('j, 'a) out, [>] as 'f, 'j * 'g lin) label -> 'h global -> 'd global
  (** Communication combinator. *)

  val gather :
    ('a list, 'b list, 'c, 'd, 'e, 'f inp_many) role ->
    ('g one, 'e one, 'h, 'c, 'b, 'i) role ->
    ('i, ('j, 'a) out, [>] as 'f, 'j list * 'g lin) label ->
    'h global -> 'd global

  val scatter :
    ('a one, 'b one, 'c, 'd, 'e, 'f inp) role ->
    ('g list, 'e list, 'h, 'c, 'b, 'i) role ->
    ('i, ('j, 'a) out_many, [>] as 'f, 'j * 'g lin) label ->
    'h global -> 'd global

  val choice_at :
    ('a one, 'b one, 'c, 'd, 'e, 'f) role ->
    ('b, 'g, 'h) disj ->
    ('g one, unit one, 'i, 'c, 'j, 'k) role * 'i global ->
    ('h one, unit one, 'm, 'c, 'n, 'o) role * 'm global ->
    'd global

  val fix : ('g global -> 'g global) -> 'g global

  val finish : ([ `cons of close one * 'a ] as 'a) global

  val finish_with_multirole :
    at:(close one, close list, [ `cons of close one * 'a ] as 'a, 'g, _, _) role ->
    'g global

  val with_multirole :
    at:(close one, close list, 'g0, 'g1, 'a, 'b) role ->
    'g0 global -> 'g1 global

  val closed_at :
    (close one, close one, 'g, 'g, 'a, 'b) role ->
    'g global -> 'g global

  val closed_list_at :
    (close list, close list, 'g, 'g, 'a, 'b) role ->
    'g global -> 'g global

  (** {2 Getting Types} *)
  
  type 'a ty

  val get_ty : ('a one, 'b, 'c, 'd, 'e, 'f) role -> 'c global -> 'a lin ty

  val get_ty_list : ('a list, 'b, 'c, 'd, 'e, 'f) role -> 'c global -> 'a lin ty

  val (>:) :
    ('obj, 'out, 'var, 'v * 'epB) label ->
    'v ty ->
    ('obj, 'out, 'var, 'v * 'epB) label
end

module type GEN = sig

  (** {2 Preamble} *)

  type 't global
  type 'a lin
  type 'a ty

  type close

  type ('a, 'b, 'c, 'e, 'f, 'g) role = ('a, 'b, 'c, 'e, 'f, 'g) Types.role
  type 'a one = 'a Types.one

  (** {1 Extracting Channel Vectors From Global Combinators} *)

  type 't tup
  (** Sequence of channels, where 't is of form [`cons of 't1 * [`cons of 't2 ...]]  *)

  val gen_with_env : Env.t -> 'a global -> 'a tup

  val gen : 'a global -> 'a tup

  val gen_mult : int list -> 'a global -> 'a tup

  val gen_with_kinds: [< `IPCProcess | `Local | `Untyped ] list -> 'a global -> 'a tup

  val gen_with_kinds_mult: ([< `IPCProcess | `Local | `Untyped ] * int) list -> 'a global -> 'a tup

  val get_ch : ('a one, 'b, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a

  val get_ch_list : ('a list, 'b, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a list

  val get_ch_ : ('a one, close one, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a * 'd tup

  val get_ch_list_ : ('a list, close one, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a list * 'd tup

  val get_ty_ : ('a one, 'b, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a lin ty

  val get_ty_list_ : ('a list, 'b, 'c, 'd, 'e, 'f) role -> 'c tup -> 'a lin ty

  val effective_length : 't tup -> int

  val env : 't tup -> Env.t
end                                   

module type PORTS =   sig

  type 't global
  
  type 'a one = 'a Types.one

  type ('a, 'b, 'c, 'e, 'f, 'g) role = ('a, 'b, 'c, 'e, 'f, 'g) Types.role

  (** {1 Shared Channels} *)

  type kind = [ `IPCProcess | `Local | `Untyped ]

  type 't shared

  val create_shared :
    ?kinds:(kind * int) list ->
    [ `cons of 'a * 'b ] global ->
    [ `cons of 'a * 'b ] shared

  val accept_ :
    [ `cons of 'a * 'b ] shared ->
    ('c one, 'd, [ `cons of 'a * 'b ], 'e, 'f, 'g) role ->
    'c IO.io

  val connect_ :
    [ `cons of 'a * 'b ] shared ->
    ('c one, 'd, [ `cons of 'a * 'b ], 'e, 'f, 'g) role ->
    'c IO.io

  val accept :
    [ `cons of 'a * 'b ] shared ->
    ('c one, 'd, [ `cons of 'a * 'b ], 'e, 'f, 'g) role ->
    'c IO.io

  val connect :
    [ `cons of 'a * 'b ] shared ->
    ('c one, 'd, [ `cons of 'a * 'b ], 'e, 'f, 'g) role ->
    'c IO.io
end
