open Base
open Common

module Make(EP:S.LIN_EP)(M:S.MONAD)(EV:S.EVENT with type 'a monad = 'a M.t) = struct

  type 'a inp =
    | InpChan of EP.once * 'a EV.event
    | InpIPC of EP.once * (unit -> tag list M.t) * (tag * (unit -> 'a M.t)) list

  let merge_in ev1 ev2 =
    match ev1, ev2 with
    | InpChan (o1, ev1), InpChan (o2, ev2) ->
       (* throw away o2 *)
       InpChan (o1, EV.choose [ev1; ev2])
    | InpIPC (o1, etag, alts1), InpIPC (o2, _, alts2) ->
       (* throw away o2 *)
       InpIPC (o1, etag, alts1 @ alts2)
    | _, _ ->
       assert false (* this won't happen since external choice is directed *)
end
