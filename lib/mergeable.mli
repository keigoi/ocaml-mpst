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
module Make(EP:S.LIN_EP) : sig
  type 'a ep = 'a EP.t
  type 'a t
  and hook = unit lazy_t

  val make : hook:hook -> mergefun:('a -> 'a -> 'a) -> value:'a ep list -> 'a t
  val make_recvar : 'a t lazy_t -> 'a t
  val make_obj_merge : ('lr,'l,'r) Base.obj_merge -> 'l t -> 'r t -> 'lr t
  val make_merge : 'a t -> 'a t -> 'a t
  val make_merge_list : 'a t list -> 'a t
  val wrap_label : (< .. > as 'l, 'v) Base.method_ -> 'v t -> 'l t
  val generate : 'a t -> 'a list
end

(* type 'a ep = 'a EP.t
 * type 'a t
 * and hook = unit lazy_t
 *
 * exception UnguardedLoop
 *
 * val make : ('a -> 'a -> 'a) -> 'a ep list -> 'a t
 * val make_with_hook : hook -> ('a -> 'a -> 'a) -> 'a ep list -> 'a t
 * val make_no_merge : 'a list -> 'a t
 * val make_recvar : 'a t lazy_t -> 'a t
 * val merge : 'a t -> 'a t -> 'a t
 * val merge_all : 'a t list -> 'a t
 * val out : 'a t -> 'a list
 * val wrap_obj : (< .. > as 'o, 'v) Base.method_ -> 'v t -> 'o t
 * val disjoint_merge : ('lr, 'l, 'r) Base.obj_merge -> 'l t -> 'r t -> 'lr t
 * end *)
