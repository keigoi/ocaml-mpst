open State

exception UnguardedLoop

type _ t

val make_deterministic : 'obj State.id -> 'obj State.t lazy_t -> 'obj t
val make_internal_choice : ('a, 'b, 'c) Rows.disj -> 'b t -> 'c t -> 'a t
val make_lazy : 'a t lazy_t -> 'a t
val unit : unit t
val merge : 'a t -> 'a t -> 'a t
val determinise : 's t -> 's
val to_string : 'a t -> string
val ensure_determinised : 's t -> 's
val determinise_core : context -> 's t -> 's t
val determinise_core_ : context -> 's t -> 's State.id * 's State.t lazy_t
val merge_core : context -> 's t -> 's t -> 's t
val force_core : context -> 'a t -> unit
val to_string_core : context -> 'a t -> string
