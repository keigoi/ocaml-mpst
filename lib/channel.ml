open Concur_shims
open Base
open Env

module Make(X:sig type 'a t and 'a u val fresh : 'a t -> 'a u end) : sig

  include S.LOCAL

  val create : ('a -> 'b) -> 'a out * 'b inp

  val create_scatter : int -> (int -> 'a -> 'b) -> 'a scatter * 'b inp list

  val create_gather : int -> ('a list -> 'b) -> 'a out list * 'b gather

  val create_untyped :
    Env.t
    -> from:int
    -> to_:int
    -> (_,_,[>] as 'var, 'v * 'cont X.u) label
    -> 'cont X.t Mergeable.t
    -> 'a out * 'var inp

  val create_untyped_scatter :
    Env.t
    -> from:int
    -> to_:int
    ->  (_,_,[>] as 'var, 'v * 'cont X.u) label
    -> 'cont X.t Mergeable.t list
    -> 'a scatter * 'var inp list

  val create_untyped_gather :
    Env.t ->
    from:int ->
    to_:int ->
    (_,_,[>] as 'var, 'v list * 'cont X.u) label ->
    'cont X.t Mergeable.t ->
    'a out list * 'var gather

end = struct

  module U = Untyped.Make(X)
  module Untyped_dpipeU = Untyped_dpipe.Make(X)
  module Untyped_streamU = Untyped_stream.Make(X)

  let (let*) = IO.bind

  type 'var inp =
    | InpName of 'var Name.inp
    | InpFun of 'var U.inp

  type 'v out =
    | OutName of 'v Name.out
    | OutFun of 'v U.out

  type 'v scatter =
    | ScatterName of 'v Name.scatter
    | ScatterFun of 'v U.out list

  type 'var gather =
    | GatherName of 'var Name.gather
    | GatherFun of 'var U.inplist

  let dpipe_table = function
    | EpDpipe ts -> Some ts
    | _ -> None

  let untyped_table = function
    | EpUntyped ts -> Some ts
    | _ -> None

  let create f =
    let out,inp = Name.create f in
    OutName out, InpName inp

  let create_scatter size f  =
    let scatter,inps = Name.create_scatter size f in
    ScatterName scatter, List.map (fun i -> InpName i) inps

  let create_gather size f =
    let outs, gather = Name.create_gather size f in
    List.map (fun o -> OutName o) outs, GatherName gather

  type ('x,'y) ch =
  | Dstream of (Untyped.tag * Obj.t) Untyped_stream.t list list
  | Dpipe of Untyped_dpipe.t list list

  let generate ~from_info ~to_info =
    match from_info.rm_kind, to_info.rm_kind with
    | EpUntyped _, _ | _, EpUntyped _->
       let dstreams =
         generate_channels
           ~newch:Untyped_streamU.create ~flipch:Untyped_streamU.flip ~tablefun:untyped_table
           ~from_info ~to_info
       in
       Dstream(dstreams)
    | EpDpipe _, _ | _, EpDpipe _ ->
       let dpipes =
         generate_channels
           ~newch:Untyped_dpipeU.new_dpipe ~flipch:Untyped_dpipeU.flip ~tablefun:dpipe_table
           ~from_info ~to_info
       in
       Dpipe(dpipes)
    | EpLocal, EpLocal ->
       assert false

  let create_untyped env ~from ~to_ label cont =
    let from_info = Env.metainfo env from 1
    and to_info = Env.metainfo env to_ 1
    in
    match generate ~from_info ~to_info with
    | Dpipe([[dpipe]]) ->
       OutFun(Untyped_dpipeU.out_dpipe dpipe label),
       InpFun(Untyped_dpipeU.(inp_dpipe (flip dpipe) label cont))
    | Dpipe(_) ->
       assert false
    | Dstream([[dstream]]) ->
       OutFun(Untyped_streamU.out_untyped dstream label),
       InpFun(Untyped_streamU.(inp_untyped (flip dstream) label cont))
    | Dstream(_) ->
       assert false

  let create_untyped_scatter env ~from ~to_ label conts =
    let from_info = Env.metainfo env from 1
    and to_info = Env.metainfo env to_ 1
    in
    match generate ~from_info ~to_info with
    | Dpipe([dpipes]) ->
       ScatterFun(List.map (fun dpipe -> Untyped_dpipeU.out_dpipe dpipe label) dpipes),
       List.map2 (fun dpipe cont -> InpFun(Untyped_dpipeU.(inp_dpipe (flip dpipe) label cont))) dpipes conts
    | Dpipe(_) ->
       assert false
    | Dstream([dstreams]) ->
       ScatterFun(List.map (fun dstream -> Untyped_streamU.out_untyped dstream label) dstreams),
       List.map2 (fun dstream cont -> InpFun(Untyped_streamU.(inp_untyped (flip dstream) label cont))) dstreams conts
    | Dstream(_) ->
       assert false

  let create_untyped_gather env ~from ~to_ label cont =
    let from_info = Env.metainfo env from 1
    and to_info = Env.metainfo env to_ 1
    in
    match generate ~from_info ~to_info with
    | Dpipe(dpipes) ->
       let dpipes = List.map List.hd dpipes in
       List.map (fun dpipe -> OutFun(Untyped_dpipeU.out_dpipe dpipe label)) dpipes,
       GatherFun(Untyped_dpipeU.(inplist_dpipe (List.map flip dpipes) label cont))
    | Dstream(dstreams) ->
       let dstreams = List.map List.hd dstreams in
       List.map (fun dstream -> OutFun(Untyped_streamU.out_untyped dstream label)) dstreams,
       GatherFun(Untyped_streamU.(inplist_untyped (List.map flip dstreams) label cont))

  let merge_out o1 o2 =
    match o1, o2 with
    | OutName o1, OutName o2 ->
       OutName (Name.merge_out o1 o2)
    | OutFun _, OutFun _ ->
       o1
    | OutName _, OutFun _ | OutFun _, OutName _ ->
       assert false

  let merge_inp i1 i2 =
    match i1, i2 with
    | InpName o1, InpName o2 ->
       InpName (Name.merge_inp o1 o2)
    | InpFun (f,wrap1), InpFun (_, wrap2) ->
       InpFun (f, U.merge_wrappers wrap1 wrap2)
    | InpName _, InpFun _ | InpFun _, InpName _ ->
       assert false

  let merge_scatter o1 o2 =
    match o1, o2 with
    | ScatterName o1, ScatterName o2 ->
       ScatterName (Name.merge_scatter o1 o2)
    | ScatterFun _, ScatterFun _ ->
       o1
    | ScatterName _, ScatterFun _ | ScatterFun _, ScatterName _ ->
       assert false

  let merge_gather (i1:'var gather) (i2:'var gather) : 'var gather =
    match i1, i2 with
    | GatherName i1, GatherName i2 ->
       GatherName (Name.merge_gather i1 i2)
    | GatherFun i1, GatherFun i2 ->
       let f1, wrap1 = i1
       and _, wrap2 = i2
       in
       GatherFun(f1, U.merge_wrappers wrap1 wrap2)
    | GatherName _, GatherFun _ | GatherFun _, GatherName _ ->
       assert false

  let[@inline] send out v =
    match out with
    | OutName out -> Name.send out v
    | OutFun f -> f v

  let[@inline] receive inp =
    match inp with
    | InpName inp -> Name.receive inp
    | InpFun (f, wrappers) ->
       IO.bind (f ()) (fun[@inline] (tag,v) ->
       IO.return (U.apply_wrapper wrappers tag v))

  let send_many outs f =
    match outs with
    | ScatterName outs -> Name.send_many outs f
    | ScatterFun outs ->
       IO_list.iteri (fun i out -> out (f i)) outs

  let receive_many inps =
    match inps with
    | GatherName inps -> Name.receive_many inps
    | GatherFun (fs, wrappers) ->
       let* xs = IO_list.map (fun f -> f ()) fs in
       let (tags, vs) = List.split xs in
       match tags with
       | tag::_ ->
          IO.return (U.apply_wrapper wrappers tag vs)
       | [] ->
          assert false
end[@@inline]
