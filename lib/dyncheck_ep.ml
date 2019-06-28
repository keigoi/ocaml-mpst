module Make(LinFlag:S.LIN_FLAG) : S.LIN_EP with type once = LinFlag.t = struct
  type once = LinFlag.t
  type 'a t = LinFlag.t -> 'a

  let make f = f

  let unrestricted v = (fun _ -> v)


  let map_merge mrg l r =
    List.map2 (fun l r f -> mrg (l f) (r @@ LinFlag.create ())) l r

  let map f ts =
    List.map (fun t once -> f (t once)) ts

  let map2 f ts1 ts2 =
    List.map2 (fun t1 t2 once -> f (t1 once) (t2 once)) ts1 ts2

  let generate fs =
    List.map (fun f -> f @@ LinFlag.create ()) fs
end
