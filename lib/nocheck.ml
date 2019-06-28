module Noflag = struct
  type t = unit
  let create _ = ()
  let use _ = ()
  exception InvalidEndpoint
end

module Nodyncheck : S.LIN_EP with type once = unit = struct
  type once = unit
  type 'a t = 'a

  let make f = f ()

  let unrestricted vs = vs

  let map f ts =
    List.map f ts

  let map2 f ts1 ts2 =
    List.map2 f ts1 ts2

  let generate fs = fs

end
