type context

type 'a head = {
  head : 'a;
  determinise_list : context -> 'a list -> 'a;
  force_traverse : context -> 'a -> unit;
  to_string : context -> 'a -> string;
}

include PolyHash.S with type t := context and type 'a value := 'a head lazy_t

val determinise_head_list :
  context -> 'a key -> 'a head lazy_t list -> 'a head lazy_t

val try_cast_then_merge_heads :
  context ->
  'a key ->
  ('b, 'a) Rows.constr ->
  ('b, 'c) Rows.constr ->
  'a head lazy_t ->
  'c head lazy_t ->
  'a head lazy_t option
