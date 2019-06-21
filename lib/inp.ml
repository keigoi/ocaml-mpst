open Base
open Common

module Make(M:S.MONAD)(E:S.EVENT with type 'a monad = 'a M.t) = struct

  type 'a inp =
    | InpChan of LinFlag.t * 'a E.event
    | InpIPC of LinFlag.t * (unit -> tag list M.t) * (tag * (unit -> 'a M.t)) list

  let merge_in ev1 ev2 =
    match ev1, ev2 with
    | InpChan (o1, ev1), InpChan (o2, ev2) ->
       LinFlag.use o1;
       LinFlag.use o2;
       InpChan (LinFlag.create (), E.choose [ev1; ev2])
    | InpIPC (o1, etag, alts1), InpIPC (o2, _, alts2) ->
       LinFlag.use o1;
       LinFlag.use o2;
       InpIPC (LinFlag.create (), etag, alts1 @ alts2)
    | _, _ ->
       assert false (* this won't happen since external choice is directed *)
end
