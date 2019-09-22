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
 * A delayed mergeable is forced when it is actually extracted by `Mergeable.resolve`.
 *
 * In ocaml-mpst, call of `Mergeable.resolve` is chained during "get_ch" phase to ensure that
 * the all mergings are resolved before actual communication will take place.
 * It is realised by the hooks -- see the implementation of (-->) combinator, for example.
 *)
type 'a t
val make : hook:unit lazy_t -> mergefun:('a -> 'a -> 'a) -> value:'a -> 'a t
val make_recvar : 'a t lazy_t -> 'a t
val make_disj_merge : ('lr,'l,'r) Base.disj_merge -> 'l t -> 'r t -> 'lr t
val make_merge : 'a t -> 'a t -> 'a t
val make_merge_list : 'a t list -> 'a t
val map : ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t
val resolve : 'a t -> 'a
