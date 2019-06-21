open Base
open Common

module Make(M:S.MONAD)(E:S.EVENT with type 'a monad = 'a M.t)
       : S.LOCAL
       with type 'a monad = 'a M.t
       with type 't out = 't Out.Make(E).out
       with type 't inp = 't Inp.Make(M)(E).inp
  = struct

  module Inp = Inp.Make(M)(E)
  module Out = Out.Make(E)

  type 'a monad = 'a M.t
  type 't out = 't Out.out
  type 't inp = 't Inp.inp

  open Inp
  open Out

  let receive = function
    | InpChan (once,ev) ->
       LinFlag.use once;
       E.sync ev
    | InpIPC (once,etag,alts) ->
       LinFlag.use once;
       (* receive tag(s) *)
       M.bind (etag ()) (fun tags ->
       let tag = List.hd tags in
       let alt = List.assoc tag alts in
       alt ())

  let size = function
    | BareOutChan chs -> List.length !chs
    | BareOutIPC fs -> List.length fs

  let send : type t u. (u one * t) out -> u -> t M.t  = fun out v ->
    let bare_out_one ch v =
      match ch with
      | BareOutChan chs ->
         E.sync (E.send (List.hd !chs) v)
      | BareOutIPC f ->
         List.hd f v
    in
    match out with
    | (Out(once,channel,(k,cont))) ->
       assert (size channel = 1);
       LinFlag.use once;
       M.bind (bare_out_one channel v) (fun _ ->
       M.return (List.nth (Mergeable.out cont) k))

  let sendmany (OutMany(once,channels,(k,cont))) vf =
    let bare_out_many ch vf =
      match ch with
      | BareOutChan chs ->
         M.iteriM (fun i ch -> E.sync (E.send ch (vf i))) !chs
      | BareOutIPC fs ->
         M.iteriM (fun i f -> f (vf i)) fs
    in
    LinFlag.use once;
    M.bind (bare_out_many channels vf) (fun _ ->
    M.return (List.nth (Mergeable.out cont) k))

  let close _ = ()
end
