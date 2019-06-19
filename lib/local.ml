open Base
type 'a inp = 'a Event.event
type 'a mrg = int * 'a Mergeable.t

let unify a b = a := !b
   
type _ out =
  | Out : LinFlag.t * 'u Event.channel list ref * 't mrg -> ('u one * 't) out
  | OutMany : LinFlag.t * 'u Event.channel list ref * 't mrg -> ('u list * 't) out
  (* | OutRemote : LinFlag.t * ('u -> unit)  * 't mrg -> ('u one * 't) out
   * | OutRemoteMany : LinFlag.t * ('u -> unit) list  * 't mrg -> ('u list * 't) out *)

let send : type t u. (u one * t) out -> u -> t  = fun out v ->
  match out with
  | (Out(once,channel,(k,cont))) ->
     assert (List.length !channel = 1);
     LinFlag.use once;
     Event.sync (Event.send (List.hd !channel) v);
     List.nth (Mergeable.out cont) k
  (* | OutRemote(once,f,(k,cont)) ->
   *    LinFlag.use once;
   *    f v;
   *    List.nth (Mergeable.out cont) k *)

let sendmany (OutMany(once,channels,(k,cont))) vf =
  LinFlag.use once;
  List.iteri (fun i ch -> Event.sync (Event.send ch (vf i))) !channels;
  List.nth (Mergeable.out cont) k

let receive ev =
  Event.sync ev

let close _ = ()

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
  (* let mergeremote (o1,f1,(i1,c1)) (o2,_,(i2,c2)) =
   *   LinFlag.use o1; LinFlag.use o2;
   *   assert (i1=i2);
   *   (LinFlag.create (), f1, (i1, Mergeable.merge c1 c2))
   * in *)
  match out1, out2 with
  | Out(a1,b1,c1), Out(a2,b2,c2) ->
     let a3,b3,c3 = mergelocal (a1,b1,c1) (a2,b2,c2) in
     Out(a3,b3,c3)
  | OutMany(a1,b1,c1), OutMany(a2,b2,c2) ->
     let a3,b3,c3 = mergelocal (a1,b1,c1) (a2,b2,c2) in
     OutMany(a3,b3,c3)
  (* | OutRemote(a1,b1,c1), OutRemote(a2,b2,c2) ->
   *    let a3,b3,c3 = mergeremote (a1,b1,c1) (a2,b2,c2) in
   *    OutRemote(a3,b3,c3)
   * | _, _ ->
   *    assert false *)

let merge_in ev1 ev2 =
  Event.choose [ev1; ev2]
       
