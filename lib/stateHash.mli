type 'a state_id

val gen_state_id : unit -> 's state_id

type ('a, 'b) eq = Eq : ('a, 'a) eq

val key_eq : 'a state_id -> 'b state_id -> ('a, 'b) eq option
val key_eq_poly : 'a state_id -> 'b state_id -> bool
val union_keys : 'a state_id -> 'a state_id -> 'a state_id

val union_keys_generalised :
  'a state_id -> 'b state_id -> ('a state_id, 'b state_id) Either.t

type 'a head = {
  head : 'a;
  merge : 'a -> 'a -> 'a;
  merge_next : dict -> 'a -> unit;
}

and dict

val add_binding : 'a state_id -> 'a head -> dict -> dict
val empty : dict
val lookup : dict -> 'a state_id -> 'a head option
