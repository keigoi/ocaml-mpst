open Base
module Make(Lin:S.LIN) : S.ENDPOINTS with type 'a lin = 'a Lin.lin =
struct
  type 'a lin = 'a Lin.lin
  type 'a t = 'a Lin.gen list Mergeable.t
  let use = Lin.use
  let[@inline] fresh t i =
    Lin.fresh (List.nth (Mergeable.resolve t) i)

  let make_lin ~hook ~mergefun ~values =
    Mergeable.make
      ~hook
      ~mergefun:(List.map2 (Lin.merge_gen mergefun))
      ~value:(List.map Lin.create values)
  let make_simple vs =
    let vs = List.map Lin.create_nolin vs in
    Mergeable.make
      ~hook:(Lazy.from_val ())
      ~mergefun:(fun _ _ -> vs)
      ~value:vs
  let wrap_label meth t =
    Mergeable.map (List.map (Lin.map_gen meth.make_obj)) (List.map (Lin.map_gen meth.call_obj)) t
  let force_merge t =
    ignore (Mergeable.resolve t)
  let fresh_all t =
    List.map Lin.fresh (Mergeable.resolve t)

  let make_recvar = Mergeable.make_recvar
  let make_merge = Mergeable.make_merge
  let make_merge_list = Mergeable.make_merge_list
  let lift_disj_merge_list mrg =
    {disj_merge=(fun ls rs ->
       List.map2 mrg.disj_merge ls rs);
     disj_splitL=(fun lr -> List.map mrg.disj_splitL lr);
     disj_splitR=(fun lr -> List.map mrg.disj_splitR lr)}

  let make_disj_merge mrg =
    Mergeable.make_disj_merge (lift_disj_merge_list @@ Lin.lift_disj_merge mrg)
end
