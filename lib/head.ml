module rec Self : sig
  type context

  type 'a head = {
    head : 'a;
    determinise_list : context -> 'a list -> 'a;
    force_traverse : context -> 'a -> unit;
    to_string : context -> 'a -> string;
  }

  include PolyHash.S with type t := context and type 'a head := 'a head
end = struct
  module Hash = PolyHash.Make (Self)
  include Hash

  type context = Hash.t

  type 'a head = {
    head : 'a;
    determinise_list : context -> 'a list -> 'a;
    force_traverse : context -> 'a -> unit;
    to_string : context -> 'a -> string;
  }
end

include Self