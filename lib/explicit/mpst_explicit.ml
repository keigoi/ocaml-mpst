include Mpst_common

type ('k, 'v, 'c) out =
  {outchan: 'k -> 'v -> unit;
   cont: 'k -> 'c Mergeable.t}
  
module MakeGlobal(X:LIN)(E:EVENT) = struct

  type ('ka,'kb,'v) channel =
     {sender: 'ka -> 'v -> unit;
      receiver: 'kb -> 'v E.event}

  type ('la,'lb,'ca,'cb,'ka,'kb,'v) slabel =
    {wraplabel: ('la,'lb, ('ka, 'v, 'ca) out X.lin, 'v * 'cb) label;
     channel: ('ka,'kb,'v) channel}
    
  let merge_send label m1 m2 =
    let m1 = X.unlin (label.obj.call_obj m1) in
    let m2 = X.unlin (label.obj.call_obj m2) in
    let cont k = Mergeable.merge (m1.cont k) (m2.cont k) in
    label.obj.make_obj (X.mklin {outchan=m1.outchan; cont})
    
  let make_send rB slab epA =
    let mergefun = merge_send slab.wraplabel in
    let outobj kt =
      slab.wraplabel.obj.make_obj
        (X.mklin
           {outchan=slab.channel.sender;
            cont=(fun _ -> Mergeable.apply epA kt)})
    in
    Mergeable.objfun mergefun rB.label outobj

  let make_recv rA lab epB =
    let ev kt k =
      E.wrap
        (E.guard (fun () -> lab.channel.receiver k))
        (fun v -> X.mklin (lab.wraplabel.var (v, Mergeable.out_ epB kt)))
    in
    Mergeable.objfun (fun x y k -> E.choose [x k;y k]) rA.label ev

  let ( --> ) : 'roleAVar 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v 'ka 'kb 'ksa 'ksb.
                (< .. > as 'roleAvar, 'kb -> 'labelvar X.lin E.event, 'ksa -> 'epA, 'ksa -> 'roleBobj, 'g1, 'g2) role ->
                (< .. > as 'roleBobj, 'labelobj,                   'ksb -> 'epB, 'ksb -> 'roleAvar, 'g0, 'g1) role ->
                (< .. > as 'labelobj, [> ] as 'labelvar, 'epA, 'epB, 'ka, 'kb, 'v) slabel ->
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

  let val__ v = Mergeable.bare_ (fun o -> assert (o=None); v)

  let make_connect rB rA2 slab epA =
    let mergefun = merge_send slab.wraplabel in
    let outobj kt =
      slab.wraplabel.obj.make_obj
        (X.mklin
           {outchan=slab.channel.sender;
            cont=(fun k -> Mergeable.apply epA (put rA2.lens kt (val__ k)))})
    in
    Mergeable.objfun mergefun rB.label outobj

  let make_accept rA rB2 lab epB =
    let ev kt k =
      E.wrap
        (E.guard (fun () -> lab.channel.receiver k))
        (fun v -> X.mklin (lab.wraplabel.var (v, Mergeable.out_ epB (put rB2.lens kt (val__ k)))))
    in
    Mergeable.objfun (fun x y k -> E.choose [x k;y k]) rA.label ev

  let ( -!-> ) : 'roleAVar 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v 'ka 'kb 'ksa1 'ksa2 'ksb1 'ksb2.
                 (< .. > as 'roleAvar, 'kb -> 'labelvar X.lin E.event, 'ksa2 -> 'epA, 'ksa1 -> 'roleBobj, 'g1, 'g2) role *
                 (_, _, unit, 'ka, 'ksa1, 'ksa2) role
                 ->
                 (< .. > as 'roleBobj, 'labelobj, 'ksb2 -> 'epB, 'ksb1 -> 'roleAvar, 'g0, 'g1) role *
                 (_, _, unit, 'kb, 'ksb1, 'ksb2) role
                 ->
                (< .. > as 'labelobj, [> ] as 'labelvar, 'epA, 'epB, 'ka, 'kb, 'v) slabel ->
                'g0 -> 'g2
    = fun (rA,rA') (rB,rB') label g0 ->
    let epB = get rB.lens g0 in
    let ev  = make_accept rA rB' label epB in
    let g1  = put rB.lens g0 ev
    in
    let epA = get rA.lens g1 in
    let obj = make_connect rB rA' label epA in
    let g2  = put rA.lens g1 obj
    in g2
end
