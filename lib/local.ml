open Base
open Common

module Make
         (EP:S.LIN_EP)
         (F:S.LIN_FLAG with type t = EP.once)
         (M:S.MONAD)
         (EV:S.EVENT with type 'a monad = 'a M.t)
       : S.LOCAL
       with type 'a monad = 'a M.t
       with type 't out = 't Out.Make(EP)(EV).out
       with type 't inp = 't Inp.Make(EP)(M)(EV).inp
  = struct

  module MA = Mergeable.Make(EP)
  module Inp = Inp.Make(EP)(M)(EV)
  module Out = Out.Make(EP)(EV)

  type 'a monad = 'a M.t
  type 't out = 't Out.out
  type 't inp = 't Inp.inp

  open Inp
  open Out

  let receive = function
    | InpChan (once,ev) ->
       F.use once;
       EV.sync ev
    | InpFun (once,etag,alts) ->
       F.use once;
       (* receive tag(s) *)
       M.bind (etag ()) (fun (tag,v) ->
       let alt = List.assoc tag alts in
       M.return (alt v))

  let size = function
    | BareOutChan chs -> List.length !chs
    | BareOutFun fs -> List.length fs

  let send : type t u. (u one * t) out -> u -> t M.t  = fun out v ->
    let bare_out_one ch v =
      match ch with
      | BareOutChan chs ->
         EV.sync (EV.send (List.hd !chs) v)
      | BareOutFun f ->
         List.hd f v
    in
    match out with
    | (Out(once,channel,(k,cont))) ->
       assert (size channel = 1);
       F.use once;
       M.bind (bare_out_one channel v) (fun _ ->
       M.return (List.nth (MA.generate cont) k))

  let sendmany (OutMany(once,channels,(k,cont))) vf =
    let bare_out_many ch vf =
      match ch with
      | BareOutChan chs ->
         M.iteriM (fun i ch -> EV.sync (EV.send ch (vf i))) !chs
      | BareOutFun fs ->
         M.iteriM (fun i f -> f (vf i)) fs
    in
    F.use once;
    M.bind (bare_out_many channels vf) (fun _ ->
    M.return (List.nth (MA.generate cont) k))

  let close _ = ()
end
