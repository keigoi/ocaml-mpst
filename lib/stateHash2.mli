type t
type 'a state_id

type 'a head = {
  head : 'a;
  determinise_all : t -> 'a list -> 'a;
  force_all : t -> 'a -> unit;
}

val make : unit -> t
val add_binding : t -> 'a state_id -> 'a head lazy_t -> unit
val lookup : t -> 'a state_id -> 'a head lazy_t option
val make_key : unit -> 'a state_id
val union_keys : 'a state_id -> 'a state_id -> 'a state_id

val general_union_keys :
  'a state_id -> 'b state_id -> ('a state_id, 'b state_id) Either.t
