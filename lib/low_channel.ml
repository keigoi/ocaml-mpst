open Concur_shims
open Types

module Make(X:sig type 'a t and 'a u val fresh : 'a t -> 'a u end) : sig

  type 'a out
  type 'a inp
  type 'a out_many
  type 'a inp_many
  
  val merge_out : 'a out -> 'a out -> 'a out
  val merge_inp : 'a inp -> 'a inp -> 'a inp
  
  val send : 'a out -> 'a -> unit IO.io
  val receive : 'a inp -> 'a IO.io
  
  val merge_out_many : 'a out_many -> 'a out_many -> 'a out_many
  val send_many : 'a out_many -> (int -> 'a) -> unit IO.io
  
  val merge_inp_many : 'a inp_many -> 'a inp_many -> 'a inp_many
  val receive_many : 'a inp_many -> 'a IO.io

  val create : ('a -> 'b) -> 'a out * 'b inp

  val create_out_many : int -> (int -> 'a -> 'b) -> 'a out_many * 'b inp list

  val create_inp_many : int -> ('a list -> 'b) -> 'a out list * 'b inp_many

  val create_untyped :
    Env.t
    -> from:int
    -> to_:int
    -> (_,_,[>] as 'var, 'v * 'cont X.u) label
    -> 'cont X.t Mergeable.t
    -> 'a out * 'var inp

  val create_untyped_out_many :
    Env.t
    -> from:int
    -> to_:int
    ->  (_,_,[>] as 'var, 'v * 'cont X.u) label
    -> 'cont X.t Mergeable.t list
    -> 'a out_many * 'var inp list

  val create_untyped_inp_many :
    Env.t ->
    from:int ->
    to_:int ->
    (_,_,[>] as 'var, 'v list * 'cont X.u) label ->
    'cont X.t Mergeable.t ->
    'a out list * 'var inp_many

end = struct
  open Env

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

  type 'v out_many =
    | OutManyName of 'v Name.out_many
    | OutManyFun of 'v U.out list

  type 'var inp_many =
    | GatherName of 'var Name.inp_many
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

  let create_out_many size f  =
    let out_many,inps = Name.create_out_many size f in
    OutManyName out_many, List.map (fun i -> InpName i) inps

  let create_inp_many size f =
    let outs, inp_many = Name.create_inp_many size f in
    List.map (fun o -> OutName o) outs, GatherName inp_many

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

  let create_untyped_out_many env ~from ~to_ label conts =
    let from_info = Env.metainfo env from 1
    and to_info = Env.metainfo env to_ 1
    in
    match generate ~from_info ~to_info with
    | Dpipe([dpipes]) ->
       OutManyFun(List.map (fun dpipe -> Untyped_dpipeU.out_dpipe dpipe label) dpipes),
       List.map2 (fun dpipe cont -> InpFun(Untyped_dpipeU.(inp_dpipe (flip dpipe) label cont))) dpipes conts
    | Dpipe(_) ->
       assert false
    | Dstream([dstreams]) ->
       OutManyFun(List.map (fun dstream -> Untyped_streamU.out_untyped dstream label) dstreams),
       List.map2 (fun dstream cont -> InpFun(Untyped_streamU.(inp_untyped (flip dstream) label cont))) dstreams conts
    | Dstream(_) ->
       assert false

  let create_untyped_inp_many env ~from ~to_ label cont =
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

  let merge_out_many o1 o2 =
    match o1, o2 with
    | OutManyName o1, OutManyName o2 ->
       OutManyName (Name.merge_out_many o1 o2)
    | OutManyFun _, OutManyFun _ ->
       o1
    | OutManyName _, OutManyFun _ | OutManyFun _, OutManyName _ ->
       assert false

  let merge_inp_many (i1:'var inp_many) (i2:'var inp_many) : 'var inp_many =
    match i1, i2 with
    | GatherName i1, GatherName i2 ->
       GatherName (Name.merge_inp_many i1 i2)
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
    | OutManyName outs -> Name.send_many outs f
    | OutManyFun outs ->
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
