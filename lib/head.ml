module rec Self : sig
  type context

  type 'a head = {
    head : 'a;
    determinise_list : context -> 'a list -> 'a;
    force_determinised : context -> 'a -> unit;
    to_string : context -> 'a -> string;
  }

  include StateHash.S with type t := context and type 'a head := 'a head
end = struct
  module Hash = StateHash.Make (Self)
  include Hash

  type context = Hash.t

  type 'a head = {
    head : 'a;
    determinise_list : context -> 'a list -> 'a;
    force_determinised : context -> 'a -> unit;
    to_string : context -> 'a -> string;
  }
end

include Self