open Base
open Common

module Make(EP:S.ENDPOINT)(M:S.MONAD)(EV:S.EVENT with type 'a monad = 'a M.t) = struct

  type 'a inp =
    | InpChan of EP.once * 'a EV.event
    | InpFun of EP.once * (unit -> (tag * Obj.t) M.t) * (tag * (Obj.t -> 'a)) list

  let merge_in ev1 ev2 =
    match ev1, ev2 with
    | InpChan (o1, ev1), InpChan (o2, ev2) ->
       (* throw away o2 *)
       InpChan (o1, EV.choose [ev1; ev2])
    | InpFun (o1, etag, alts1), InpFun (o2, _, alts2) ->
       (* throw away o2 *)
       InpFun (o1, etag, alts1 @ alts2)
    | _, _ ->
       assert false
end
