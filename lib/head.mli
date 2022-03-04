type context

type 'a head = {
  head : 'a;
  determinise_list : context -> 'a list -> 'a;
  force_traverse : context -> 'a -> unit;
  to_string : context -> 'a -> string;
}

include PolyHash.S with type t := context and type 'a head := 'a head
