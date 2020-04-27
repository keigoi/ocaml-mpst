open Concur_shims

module type COMM = sig

  (** {2 Channel Types}  *)

  (** 
    Channels in [ocaml-mpst] are nested inside objects, 
    reflecting {i communication protocols} projected from a {i global combinator} ({! global}). 
    Channels have three modes, {b output}, {b input} and {b closing}, and may 
    also have {b loops}, as follows:

    {[
      Mode   | Type                                                                         |
      -------+------------------------------------------------------------------------------+
      Output | <role_R: <label1: ('v1,'t1) out; ...; labeln: ('vn,'tn) out> >             |
      Input  | <role_R: [`label1 of 'v1*'t1; ...; `labeln of 'vn*'tn] inp >             |
      Close  | close                                                                        |
      Loop   | (t as 'a), where 'a is bound to the loop body t itself (equi-recursion)      | 
    ]}

    Output and input are wrapped by destination {b role} (denoted by [role_R]) showing from/to which
    peer it should receive/send on that channel, respectively.
    Output is again nested inside {b labels} [label1 .. labeln].
    Then, {b bare output channel} {! out} of form [('v,'t) out] denotes an output of {b payload} ['v] 
    and a {b continuation} (i.e. next state), which can be performed by {! send} primitive below. 
    Thus, to send something on a channel [s] to role [Dst] with [labelX] and [payload], 
    you will write as follows:

      {[send s#role_Dst#labelX payload]}

    It is parsed as [(send (s#role_Dst#labelx) payload)]. 
    Furthermore, to follow the protocol on [s], the best practice is to bind the continuation
    to the same variable [s]. Thus, the sender side will look like:

      {[let s = send s#role_Dst#labelX payload in ...]}

    On the other hand, a {b bare input channel} {! inp} of form[[`label1 of 'v1*'t1|…] inp] is 
    directly inside the destination role. The label chosen by the sender appears as 
    a variant tag received by {! receive} primitive, having both a payload ['v] and 
    the continuation ['t] the receiver must follow.
    The typical receiver's code will look like:
    
    {[
      match s#role_R with
      | `label1(x, s) -> ...
      | ...
      | `labeln(x, s) -> ...
    ]}

    (In case you are using {i lwt}, you must first bind the retuned value then pattern-match on
    it, like the following:
    
    {[
      let* var = receive s#role_R in 
      match var with | `label1(x, s) -> ...
    ]}
  
    where [let*] is defined like [let (let* ) = Lwt.bind].)

    In case a single tag is expected, you may write as follows:

    {[
      let `label(x, s) = receive s#role_R in ...
    ]}

    On closing, [close] primitive will not return anything but a unit value. 
    Thus, the channel will be closed as follows:

    {[
      let () = close s in ...
    ]}
    or, just:
    {[ 
      close s
    ]}

    Loops are handled transparently, thanks to equi-recursive nature of
    recursive types in OCaml.
    
      Communication can be synchronous (i.e. output is blocking)  or
      asynchronous (i.e. output is non-blocking), depending on the underlying communication medium.
    *)

  exception InvalidEndpoint
  (** 
    Raised if linearity is violated (i.e. a channel of the same state is used more than once).
   *)

  type ('v, 's) out
  (** Output of ['v] then continue to session ['s]. *)

  type 'var inp
  (** Input of type ['var] having the form of 
      [[`label1 of 'v1 * 's1 | ... | `labeln of 'vn * 'sn]].
    *)

  type close
  (** Termination of a session. *)

  type ('v, 's) out_many
  (** Output of 'v to multiple peers. See [scatter] in the global combinator section. *)

  type 'var inp_many
  (** Input of type ['var] from multiple peers, where ['var] is of form
      [[`label1 of 'v1 list * 's1 | ... ]].
      See [gather] in the global combinator section. 
    *)

  (** {2 Primitives} *)

  val send : ('v, 't) out -> 'v -> 't IO.io
  (** [send b v] outputs a value [v] on the bare output channel [b], 
      returning a continuation. 
      @raise InvalidEndpoint The channel is already consumed (i.e. linearity is violated).
    *)

  val receive : 'var inp -> 'var IO.io
  (** [receive b] receives on [b] a value (wrapped in a variant tag).
      @raise InvalidEndpoint
    *)

  val close : close -> unit IO.io
  (** Close the channel. 
      @raise InvalidEndpoint
    *)

  val send_many : ('v, 't) out_many -> (int -> 'v) -> 't IO.io
  (**
      [send_many b f] outputs values [f i] to multiple peers
      where [i] is the index of the peer.
      @raise InvalidEndpoint
    *)
  
  val receive_many : 'var inp_many -> 'var IO.io
  (**
      [receive_many b] inputs values from multiple peers.
      @raise InvalidEndpoint
    *)
end  

module type GLOBAL_COMBINATORS = sig


  (**/**)

  type ('v,'t) out
  type 'var inp
  type ('v,'t) out_many
  type 'var inp_many
  type close

  (**/**)

  (** {2 Types} *)
     
  type 't global
  (** Type of a global protocol specification. In type ['t global], ['t] is of form 
      [[`cons of 't1 * [`cons of 't2 * ...]]] 
      where ['t1] is the channel type for the 1st role, ['t2] for the 2nd role, etc.
    *)

  type 'a lin
  (** Linear type constructor, which is expanded to ['a] when linearity is dynamically checked,
      and ['a Linocaml.lin] when it is statically checked via Linocaml.
    *)

  (**/**)

  type ('obj,'ot,'var,'vt) label = ('obj,'ot,'var,'vt) Types.label 

  type ('ts, 't, 'us, 'u, 'robj, 'mt) role = ('ts, 't, 'us, 'u, 'robj, 'mt) Types.role

  type ('lr, 'l, 'r) disj = ('lr, 'l, 'r) Types.disj

  type 'a one = 'a Types.one

  (**/**)

  (** {2 Combinators} *)

  (** {b Communication combinator} declaring a transmission of a single label (and a payload). 
      The protocol [((ri --> rj) lab g)] is a transmission of label [lab] from [ri] to [rj], 
      then continues to [g] (the payload type is inferred by typechecker).
    *)
  val ( --> ) :
    ('si one, 'rj one, 'mid, 'cur, 'ri, 'var inp) role ->
    ('sj one, 'ri one, 'nxt, 'mid, 'rj, 'obj) role ->
    ('obj, ('v, 'si) out, [>] as 'var, 'v * 'sj lin) label -> 'nxt global -> 'cur global
  (** The following type signature of {! (-->)} embodies {b endpoint projection} of MPST theory 
      from global types to local types, in flavour of type-manipulation techs developed above.

      {[
      val ( --> ) :
        ('si one, 'rj one, 'mid, 'cur, 'ri, 'var inp) role ->
        ('sj one, 'ri one, 'nxt, 'mid, 'rj, 'obj) role ->
        ('obj, ('v, 'si) out, [>] as 'var, 'v * 'sj lin) label -> 'nxt global -> 'cur global
      ]}

      (Hereafter, we assume that the indices of [ri] and [rj] are i-th and j-th respectively.)
      
      In [(ri --> rj) lab g], the type ['nxt] in [(g : 'nxt global)] has the form:

      {[
        ['s0; ...; 'si; ...; 'sj; ...]
      ]}

      which is the type-level list ([[`cons of ...]]) written in the OCaml's list syntax,
      where [s0], [si] and [sj] are the first, i-th and j-th element in ['nxt].

      This is firstly updated to ['mid], having the following form:

      {[
        ['s0; ...; 'si; ...; <role_Ri:[> `lab of 'v*'sj] inp>; ...]
      ]}

      and then updated to ['cur]:

      {[
        ['s0; ...; <role_Rj:<lab:('v,'si) out>>; ...; <role_Ri:[>`lab of 'v*'sj] inp>; ...]
      ]}

      The resulting type ['cur] says that:
      - Role [Ri] outputs to [Rj] with label [lab] and payload ['v], then continues to [si], 
      - Role [Rj] inputs from [Ri] with label [lab] and payload ['v], then continues to [sj], and
      - The behaviour of the rest of roles are unchanged.
      
      Which exactly follows the end point projection from a single-labelled global type.
      
      The type {! one} says that it is one-to-one communication, as opposed to 
      {! scatter} and {! gather} developed later.
    *)

  (** Many-to-one communication. *)
  val gather :
    ('a list, 'b list, 'c, 'd, 'e, 'f inp_many) role ->
    ('g one, 'e one, 'h, 'c, 'b, 'i) role ->
    ('i, ('j, 'a) out, [>] as 'f, 'j list * 'g lin) label ->
    'h global -> 'd global

  (** One-to-many communication. *)
  val scatter :
    ('a one, 'b one, 'c, 'd, 'e, 'f inp) role ->
    ('g list, 'e list, 'h, 'c, 'b, 'i) role ->
    ('i, ('j, 'a) out_many, [>] as 'f, 'j * 'g lin) label ->
    'h global -> 'd global

  (** {b Branching combinator} declares a binary choice in a protocol.
      In protocol [(choice_at r disj (r,g1) (r,g2))], role [r] decides the branching between [g1] and [g2],
      where [disj] is the concatenation {!disj} of two outputs at [r] in [g1] and [g2].
      The three occurrences of [r] must be the same.
   *)
  val choice_at :
    (unit one, 'r one, 'mid, 'cur, 'r, _) role ->
    ('r, 'rL, 'rR) disj ->
    ('rL one, unit one, 'nxtL, 'mid, 'rL, _) role * 'nxtL global ->
    ('rR one, unit one, 'nxtR, 'mid, 'rR, _) role * 'nxtR global ->
    'cur global
  (**
    {[
      val choice_at :
        (unit one, 'r one, 'mid, 'cur, 'r, _) role ->
        ('r, 'rL, 'rR) disj ->
        ('rL one, unit one, 'nxtL, 'mid, 'rL, _) role * 'nxtL global ->
        ('rR one, unit one, 'nxtR, 'mid, 'rR, _) role * 'nxtR global ->
        'cur global

    ]}
  *)

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

  (**/**)

  type 't global
  type 'a lin
  type 'a ty
  type close
  type ('a, 'b, 'c, 'e, 'f, 'g) role = ('a, 'b, 'c, 'e, 'f, 'g) Types.role
  type 'a one = 'a Types.one

  (**/**)

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

  (**/**)

  type 't global
  type 'a one = 'a Types.one
  type ('a, 'b, 'c, 'e, 'f, 'g) role = ('a, 'b, 'c, 'e, 'f, 'g) Types.role

  (**/**)

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
