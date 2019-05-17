include Mpst_common
open Guarded


module Make(X:sig type conn end) = struct
  type conn = X.conn
  module ConnTable : sig
    type t
    val create : unit -> t
    val getone : t -> 'k -> conn
    val putone : t -> 'k -> conn -> t
    val getmany : t -> 'k -> conn list
    val putmany : t -> 'k -> conn list -> t
  end = struct
    type t = (Obj.t * conn list) list
    let create () = []
    let putmany t key ks = (Obj.repr key,ks)::t
    let getmany t key = List.assoc (Obj.repr key) t
    let putone t key k = (Obj.repr key,[k])::t
    let getone t key =
      match List.assoc (Obj.repr key) t with
      | [] -> raise Not_found
      | [x] -> x
      | _ -> failwith "ConnTable: multiplicity mismatch"
  end

  module Sess = struct
    type t = ConnTable.t
  end
  include Seq(Sess)

  type 'v channel =
    {sender: conn -> 'v -> unit;
     receiver: conn -> 'v Event.event}

  type ('v, 'k) out =
    {outchan: 'v -> unit;
     cont: 'k prot}
      
  module MakeGlobal(X:LIN) = struct

    type ('la,'lb,'ca,'cb,'v1,'v2) slabel =
      {wraplabel:('la,'lb, ('v1, 'ca) out X.lin, 'v2 * 'cb) label;
       channel: 'v1 channel}
    
    let make_send rB lab epA =
      let method_ obj kt =
        match obj with
        | None ->
           X.mklin
             {outchan=(fun v -> lab.channel.sender (ConnTable.getone kt rB.lens) v);
              cont=epA}
        | Some obj ->
           let obj = obj kt in
           let {cont=epA'; outchan} =
             X.unlin (lab.wraplabel.call_obj (rB.label.call_obj obj))
           in
           X.mklin
             {outchan;
              cont=prot_merge epA epA'}
      in
      val_ @@ (fun obj kt ->
                  rB.label.make_obj (lab.wraplabel.make_obj (method_ obj kt)))

    let make_recv rA lab epB =
      let method_ obj kt =
        let ev cont =
          Event.wrap
            (Event.guard (fun () ->
                 lab.channel.receiver (ConnTable.getone kt rA.lens)))
            (fun v -> X.mklin (lab.wraplabel.make_var (v, unprot kt cont)))
        in
        match obj with
        | None ->
           ev epB
        | Some obj ->
           let obj = obj kt in
           Event.choose [ev epB; rA.label.call_obj obj]
      in
      val_ @@ (fun obj kt ->
                  rA.label.make_obj (method_ obj kt))

    let ( --> ) : 'roleAVar 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
                  (< .. > as 'roleAvar, _, 'labelvar X.lin Event.event, 'epA, 'roleBobj, 'g1, 'g2) role ->
                  (< .. > as 'roleBobj, _, 'labelobj,             'epB, 'roleAvar, 'g0, 'g1) role ->
                  (< .. > as 'labelobj, [> ] as 'labelvar, 'epA, 'epB, 'v, 'v) slabel ->
                  'g0 -> 'g2
      = fun rA rB label g0 ->
      let epB = get rB.lens g0 in
      let ev  = make_recv rA label epB in
      let g1  = put rB.lens g0 ev
      in
      let epA = get rA.lens g1 in
      let obj = make_send rB label epA in
      let g2  = put rA.lens g1 obj
      in g2
  end
end
