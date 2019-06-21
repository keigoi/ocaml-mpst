open Base
open Common

module Make(E:S.EVENT) = struct
  type 'v bare_out =
    | BareOutChan of 'v E.channel list ref
    | BareOutIPC of ('v -> unit E.monad) list

  type _ out =
    | Out : LinFlag.t * 'u bare_out * (int * 't Mergeable.t) -> ('u one * 't) out
    | OutMany : LinFlag.t * 'u bare_out * (int * 't Mergeable.t) -> ('u list * 't) out

  let unify a b =
    match a,b with
    | BareOutChan(a), BareOutChan(b) -> a := !b
    | BareOutIPC(_), BareOutIPC(_) -> ()
    | _, _ -> assert false

  let merge_out : type u t. (u * t) out -> (u * t) out -> (u * t) out =
    fun out1 out2 ->
    let mergelocal (o1,s1,(i1,c1)) (o2,s2,(i2,c2)) =
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
