module Noflag = struct
  type t = unit
  let create _ = ()
  let use _ = ()
  exception InvalidEndpoint
end

module Nodyncheck_nodelay : Mpst.S.LIN_EP with type once = unit = struct
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

(* a delayyed version (will be removed when we fix the bug) *)
module Nodyncheck : Mpst.S.LIN_EP with type once = unit = struct
  type once = unit
  type 'a t = unit -> 'a

  let make f = f

  let unrestricted vs = List.map (fun v _ -> v) vs

  let map_merge mrg l r =
    List.map2 (fun l r _ -> mrg (l ()) (r ())) l r

  let map f ts =
    List.map (fun t _ -> (f (t ()))) ts

  let map2 f ts1 ts2 =
    List.map2 (fun t1 t2 _ -> f (t1 ()) (t2 ())) ts1 ts2

  let out fs = List.map (fun f -> f ()) fs

end
