module Noflag = struct
  type t = unit
  let create _ = ()
  let use _ = ()
  exception InvalidEndpoint
end

module Nodyncheck = struct
  type once = unit
  type 'a t = 'a

  let make f = f ()

  let unrestricted vs = vs

  let map_merge mrg l r =
    List.map2 mrg l r

  let map f ts =
    List.map f ts

  let map2 f ts1 ts2 =
    List.map2 f ts1 ts2

  let out fs = fs

end
