type tag = int

module type S = sig
  type 'a name
  type 'a endpoint
  type chan

  val new_name : chan -> 'a name
  val unify : 'a name -> 'a name -> unit
  val finalise : 'a name -> 'a endpoint

  type 'a out
  and 'var inp

  val out :
    ('a, 'b) Rows.method_ ->
    ('b, 'c out) Rows.method_ ->
    tag name ->
    'c State.t ->
    'a State.t

  val inp :
    ('a, 'c inp) Rows.method_ ->
    ('c, 'd) Rows.constr ->
    tag name ->
    'd State.t ->
    'a State.t

  val make : unit -> chan
  val select : 'a out -> 'a
  val branch : 'a inp -> 'a
end

module Sync : S with type chan = unit
module Async : S with type chan = DynChan.chan
