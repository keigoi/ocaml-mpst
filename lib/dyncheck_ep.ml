module Make(DynLinFlag:S.DYN_LIN_FLAG) : S.ENDPOINT with type once = DynLinFlag.t = struct
  type once = DynLinFlag.t
  type 'a t = DynLinFlag.t -> 'a

  let make f = f

  let unrestricted v = (fun _ -> v)

  let map f ts =
    List.map (fun t once -> f (t once)) ts

  let map2 f ts1 ts2 =
    List.map2 (fun t1 t2 once -> f (t1 once) (t2 once)) ts1 ts2

  let generate fs =
    List.map (fun f -> f @@ DynLinFlag.create ()) fs
end
