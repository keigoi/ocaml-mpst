open Base
open Common

module type LIN = sig
  type 'a lin
  val mklin : 'a -> 'a lin
  val unlin : 'a lin -> 'a
end

module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val return_unit : unit t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val iteriM : (int -> 'a -> unit t) -> 'a list -> unit t
  val mapM : ('a -> 'b t) -> 'a list -> 'b list t
end

module type EVENT = sig
  type 'a event
  type 'a channel
  val new_channel : unit -> 'a channel
  val flip_channel : 'a channel -> 'a channel
  val receive : 'a channel -> 'a event
  val send : 'a channel -> 'a -> unit event
  val guard : (unit -> 'a event) -> 'a event
  val choose : 'a event list -> 'a event
  val wrap : 'a event -> ('a -> 'b) -> 'b event
  val always : 'a -> 'a event
  val receive_list : 'a channel list -> 'a list event

  type 'a monad
  val sync : 'a event -> 'a monad
end

module type SERIAL = sig
  type 'a monad
  type out_channel
  type in_channel
  val output_tag : out_channel -> tag -> unit monad
  val output_value : out_channel -> 'v -> unit monad
  val input_tag : in_channel -> tag monad
  val input_value : in_channel -> 'v monad
  val input_value_list : in_channel list -> 'v list monad
  val flush : out_channel -> unit monad
  val pipe : unit -> (in_channel * out_channel)
end

module type LIN_FLAG = sig
  type t
  val create     : unit -> t
  val use        : t -> unit
  val try_use    : t -> bool
  exception InvalidEndpoint
end

(**
 * A mergeable is a session endpoint which can be merged with another endpoint in future.
 * A mergeble is a bundle of an endpoint and its merging strategy, providing a way 
 * to merge two endpoints of the same type into one.
 * 
 * Mergeables enable endpoints to be merged based on the object structure.
 * In ocaml-mpst, endpoints are nested objects (and events).
 * The problem is that, in OCaml type system, one can not inspect the object structure
 * when its type is not known, and because of this, we can not merge structural objects. 
 * (In this case, I think GADTs will not help.) 
 * As mergeables have both merging strategy and its value at the same place,
 * it enables values to be merged with other values if they have the same type.
 *
 * Mergeables themselves can be merged with other mergeables using `Mergeable.merge`.
 *
 * Mergeables can be "delayed" by using `Mergeable.make_recvar`. Delayed mergeables are used
 * to encode recursive endpoints. Merging delayed mergeable will also generate a delayed 
 * mergeable.
 * A delayed mergeable are forced when it is actually extracted by `Mergeable.out`.
 *
 * In ocaml-mpst, call of `Mergeable.out` is chained during "get_ep" phase to ensure that 
 * the all mergings are resolved before actual communication will take place.
 * It is realised by the hooks -- see the implementation of (-->) combinator, for example.
 *)
module type MERGEABLE =
sig
  type 'a t
  and hook = unit lazy_t

  exception UnguardedLoop

  val make : ('a -> 'a -> 'a) -> 'a ep list -> 'a t
  val make_with_hook : hook -> ('a -> 'a -> 'a) -> 'a ep list -> 'a t
  val make_no_merge : 'a list -> 'a t
  val make_recvar : 'a t lazy_t -> 'a t
  val merge : 'a t -> 'a t -> 'a t
  val merge_all : 'a t list -> 'a t
  val out : 'a t -> 'a list
  val wrap_obj : (< .. > as 'o, 'v) method_ -> 'v t -> 'o t
  val disjoint_merge : ('lr, 'l, 'r) obj_merge -> 'l t -> 'r t -> 'lr t
end

(**
 * The module for sequences of mergeables (endpoints).
 *)
module type SEQ =
sig
  type 'a mergeable

  (** sequence type *)
  type _ t = (* can we hide these constructors? *)
    (** cons *)
    | SeqCons : 'hd mergeable * 'tl t -> [ `cons of 'hd * 'tl ] t
    (** repetition -- for closed endpoints *)
    | SeqRepeat : int * (int -> 'a mergeable) -> ([ `cons of 'a * 'b ] as 'b) t
    (** recursion variable(s) *)
    | SeqRecVars : 'a seqvar list -> 'a t
    (** unguarded loop; we have it in the last part of a recursion *)
    | SeqBottom : 'x t
  and 'a seqvar = 'a t lazy_t

  (** lenses in constructor form *)
  type (_, _, _, _) lens =
      Zero : ('hd0, 'hd1, [ `cons of 'hd0 * 'tl ] t, [ `cons of 'hd1 * 'tl ] t) lens
    | Succ :
        ('a, 'b, 'tl0 t, 'tl1 t) lens
        -> ('a, 'b, [ `cons of 'hd * 'tl0 ] t, [ `cons of 'hd * 'tl1 ] t) lens

  (** raised when one would try to extract a value from unguarded loop *)
  exception UnguardedLoopSeq

  (** lens get function *)
  val get : ('a, 'b, 'xs, 'ys) lens -> 'xs -> 'a mergeable
  (** lens put function *)
  val put : ('a, 'b, 'xs, 'ys) lens -> 'xs -> 'b mergeable -> 'ys

  (** lens get function *)
  val int_of_lens : ('a, 'b, 'xs, 'ys) lens -> int

  (** merging of two sequences in a choice  *)
  val seq_merge : 'x t -> 'x t -> 'x t

  (**
   * partial_force:
   * it tries to expand unguarded recursion variables which occurs right under the
   * fixpoint combinator. This enables a "fail-fast" policy to handle unguarded recursions --
   * it would raise an exception if there is an unguarded occurrence of a recursion variable.
   * This fuction is called during the initial construction phase of an 
   * endpoint sequence (fix).
   *)
  val partial_force : 'x t lazy_t list -> 'x t -> 'x t
end
