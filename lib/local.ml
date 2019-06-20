open Base
type 'a mrg = int * 'a Mergeable.t

module MakeChan(Event:S.EVENT) = struct

  type 'a inp =
    | InpChan of LinFlag.t * 'a Event.event
    | InpIPC of LinFlag.t * tag list Event.event * (tag * 'a Event.event) list

  type 'v bare_out =
    | BareOutChan of 'v Event.channel list ref
    | BareOutIPC of ('v -> unit) list

  type _ out =
    | Out : LinFlag.t * 'u bare_out * 't mrg -> ('u one * 't) out
    | OutMany : LinFlag.t * 'u bare_out * 't mrg -> ('u list * 't) out

  let merge_in ev1 ev2 =
    match ev1, ev2 with
    | InpChan (o1, ev1), InpChan (o2, ev2) ->
       LinFlag.use o1;
       LinFlag.use o2;
       InpChan (LinFlag.create (), Event.choose [ev1; ev2])
    | InpIPC (o1, etag, alts1), InpIPC (o2, _, alts2) ->
       LinFlag.use o1;
       LinFlag.use o2;
       InpIPC (LinFlag.create (), etag, alts1 @ alts2)
    | _, _ ->
       assert false (* this won't happen since external choice is directed *)

  let unify a b =
    match a,b with
    | BareOutChan(a), BareOutChan(b) -> a := !b
    | BareOutIPC(_), BareOutIPC(_) -> ()
    | _, _ -> assert false
                                                                                   
  let merge_out : type u t. (u * t) out -> (u * t) out -> (u * t) out =
    fun out1 out2 ->
    let mergelocal  (o1,s1,(i1,c1)) (o2,s2,(i2,c2)) =
      assert (i1=i2);
      LinFlag.use o1; LinFlag.use o2;
      unify s1 s2;
      let o12 = LinFlag.create () in
      let c12 = Mergeable.merge c1 c2 in
      (o12, s1, (i1, c12))
    in
    match out1, out2 with
    | Out(a1,b1,c1), Out(a2,b2,c2) ->
       let a3,b3,c3 = mergelocal (a1,b1,c1) (a2,b2,c2) in
       Out(a3,b3,c3)
    | OutMany(a1,b1,c1), OutMany(a2,b2,c2) ->
       let a3,b3,c3 = mergelocal (a1,b1,c1) (a2,b2,c2) in
       OutMany(a3,b3,c3)

end

module Make(M:S.MONAD)(Event:S.EVENT with type 'a monad = 'a M.t) = struct
  module Chan = MakeChan(Event)
  open Chan
     
  let receive = function
    | InpChan (once,ev) ->
       LinFlag.use once;
       Event.sync ev
    | InpIPC (once,etag,alts) ->
       LinFlag.use once;
       (* receive tag(s) *)
       M.bind (Event.sync etag) (fun tags ->
           let tag = List.hd tags in
       Event.sync (List.assoc tag alts))

  let size = function
    | BareOutChan chs -> List.length !chs
    | BareOutIPC fs -> List.length fs

  let send : type t u. (u one * t) out -> u -> t M.t  = fun out v ->
    let bare_out_one ch v =
      match ch with
      | BareOutChan chs ->
         Event.sync (Event.send (List.hd !chs) v)
      | BareOutIPC f -> List.hd f v; M.return_unit
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
         M.iteriM (fun i ch -> Event.sync (Event.send ch (vf i))) !chs
      | BareOutIPC fs ->
         List.iteri (fun i f -> f (vf i)) fs;
         M.return_unit
    in
    LinFlag.use once;
    M.bind (bare_out_many channels vf) (fun _ ->
    M.return (List.nth (Mergeable.out cont) k))

  let close _ = ()
end  
