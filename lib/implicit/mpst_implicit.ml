include Mpst_common

module Make(X:Mpst_base.S.RAW)(E:EVENT) = struct
  type conn = X.conn
  module Connection = Mpst_base.Connection.Make(X)
  module ConnTable = Connection.ConnTable

  type 'v channel =
    {sender: conn -> 'v -> unit;
     receiver: conn -> 'v E.event}

  type ('v, 'k) out =
    {outchan: 'v -> unit;
     cont: 'k Mergeable.t}

  let protclose _ _ = Close

  let finish : ([`cons of (ConnTable.t -> close) * 'a] as 'a) Seq.t =
    SeqRepeat(Mergeable.make_no_merge (fun _ -> Close))

  let choice_at : 'k 'ep 'ep_l 'ep_r 'g0_l 'g0_r 'g1 'g2.
                  (_, _, unit, ConnTable.t -> (< .. > as 'ep), 'g1 Seq.t, 'g2 Seq.t) role ->
                  ('ep, < .. > as 'ep_l, < .. > as 'ep_r) obj_merge ->
                  (_, _, ConnTable.t -> 'ep_l, unit, 'g0_l Seq.t, 'g1 Seq.t) role * 'g0_l Seq.t ->
                  (_, _, ConnTable.t -> 'ep_r, unit, 'g0_r Seq.t, 'g1 Seq.t) role * 'g0_r Seq.t ->
                  'g2 Seq.t
    = fun r merge (r',g0left) (r'',g0right) ->
    let epL, epR =
      Seq.get r'.lens g0left,
      Seq.get r''.lens g0right in
    let g1left, g1right =
      Seq.put r'.lens g0left (Mergeable.make_no_merge ()),
      Seq.put r''.lens g0right (Mergeable.make_no_merge ()) in
    let g1 = Seq.seq_merge g1left g1right in
    let mrgfun =
      {obj_merge=(fun l r -> (fun kt -> merge.obj_merge (l kt) (r kt)));
       obj_splitL=(fun lr -> (fun kt -> merge.obj_splitL (lr kt)));
       obj_splitR=(fun lr -> (fun kt -> merge.obj_splitR (lr kt)));
      }
    in
    let ep = Mergeable.disjoint_merge mrgfun epL epR in
    let g2 = Seq.put r.lens g1 ep
    in
    g2

  module MakeGlobal(X:LIN) = struct

    type ('la,'lb,'ca,'cb,'v1,'v2) slabel =
      {wraplabel:('la,'lb, ('v1, 'ca) out X.lin, 'v2 * 'cb) label;
       channel: 'v1 channel}

    let merge_send label m1 m2 =
      let m1 = X.unlin (label.obj.call_obj m1) in
      let m2 = X.unlin (label.obj.call_obj m2) in
      let cont = Mergeable.merge m1.cont m2.cont in
      label.obj.make_obj (X.mklin {outchan=m1.outchan; cont})

    let make_send rB slab epA =
      let mergefun = merge_send slab.wraplabel in
      let outobj kt =
        slab.wraplabel.obj.make_obj
          (X.mklin
             {outchan=(fun v -> slab.channel.sender (ConnTable.getone kt rB.lens) v);
              cont=Mergeable.apply epA kt})
      in
      Mergeable.objfun mergefun rB.label outobj

    let make_recv rA lab epB =
      let ev kt =
        E.wrap
          (E.guard (fun () ->
               lab.channel.receiver (ConnTable.getone kt rA.lens)))
          (fun v -> X.mklin (lab.wraplabel.var (v, Mergeable.out epB kt)))
      in
      Mergeable.objfun (fun x y -> E.choose [x;y]) rA.label ev

    let ( --> ) : 'roleAVar 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
                  (< .. > as 'roleAvar, 'labelvar X.lin E.event, ConnTable.t -> 'epA, ConnTable.t -> 'roleBobj, 'g1, 'g2) role ->
                  (< .. > as 'roleBobj, 'labelobj,                   ConnTable.t -> 'epB, ConnTable.t -> 'roleAvar, 'g0, 'g1) role ->
                  (< .. > as 'labelobj, [> ] as 'labelvar, 'epA, 'epB, 'v, 'v) slabel ->
                  'g0 -> 'g2
      = fun rA rB label g0 ->
      let epB = Seq.get rB.lens g0 in
      let ev  = make_recv rA label epB in
      let g1  = Seq.put rB.lens g0 ev
      in
      let epA = Seq.get rA.lens g1 in
      let obj = make_send rB label epA in
      let g2  = Seq.put rA.lens g1 obj
      in g2
  end

  include MakeGlobal(struct type 'a lin = 'a let mklin x = x let unlin x = x end)
end

module IPC = struct
  module Raw = Mpst_base.Raw_unixpipe
  include Make(Raw)(LwtEvent)
  module Connection_ = Mpst_base.Connection.Make(Raw)
  open Connection_
  open Raw

  let msg =
    {wraplabel=msg;
     channel={
         sender=(fun k -> write (fun v -> `msg(v)) k);
         receiver=(fun k -> try_read (function `msg(v) -> Some(v) | _ -> None) k)
    }}

  let left =
    {wraplabel=left;
     channel={
         sender=(fun k -> write (fun v -> `left(v)) k);
         receiver=(fun k -> try_read (function `left(v) -> Some(v) | _ -> None) k)
    }}

  let right =
    {wraplabel=right;
     channel={
         sender=(fun k -> write (fun v -> `right(v)) k);
         receiver=(fun k -> try_read (function `right(v) -> Some(v) | _ -> None) k)
    }}
end
let close Close = ()

(* type conn = Mpst_base.Raw_unixpipe.conn *)
