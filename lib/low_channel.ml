open Concur_shims
open Types

module Make(X:sig type 'a t and 'a u val fresh : 'a t -> 'a u val unfresh_ : 'a u -> 'a t end) : sig

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

  val create : ('var, 'v * 'c X.u) constr -> 'c X.t Mergeable.t -> 'v out * 'var inp

  val create_out_many : ('var, 'v * 'c X.u) constr -> 'c X.t Mergeable.t list -> 'v out_many * 'var inp list

  val create_inp_many : int -> ('var, 'v list * 'c X.u) constr -> 'c X.t Mergeable.t -> 'v out list * 'var inp_many

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
  module Wrapper = Wrapper.Make(X)

  let (let*) = IO.bind

  type 'var inp0 =
    | InpName of 'var Name.inp
    | InpFun of 'var U.inp

  type 'var inp = {inp:'var inp0; wrappers: 'var Wrapper.t list}

  type 'v out =
    | OutName of 'v Name.out
    | OutFun of 'v U.out

  type 'v out_many =
    | OutManyName of 'v Name.out_many
    | OutManyFun of 'v U.out list

  type 'var inp_many0 =
    | GatherName of 'var Name.inp_many
    | GatherFun of 'var U.inplist

  type 'var inp_many = {inp_many:'var inp_many0; wrappers_many: 'var Wrapper.t list}

  let dpipe_table = function
    | EpDpipe ts -> Some ts
    | _ -> None

  let untyped_table = function
    | EpUntyped ts -> Some ts
    | _ -> None
 
  let create : 'var 'v 'c. ('var, 'v * 'c X.u) constr -> 'c X.t Mergeable.t -> 'v out * 'var inp = fun constr cont ->
    let wrapperfun, wrapper = Wrapper.make constr cont in
    let out,inp0 = Name.create (fun v -> !wrapperfun v) in
    OutName out, {inp=InpName inp0; wrappers=[wrapper]}
  
  let create_out_many : 'var 'v 'c. ('var, 'v * 'c X.u) constr -> 'c X.t Mergeable.t list -> 'v out_many * 'var inp list = 
    fun constr conts ->
    let wrapperfuns, wrappers = List.split @@ List.map (fun cont -> Wrapper.make constr cont) conts in
    let size = List.length wrappers in
    let out_many, inps = Name.create_out_many size (fun i -> let wrapperfun = List.nth wrapperfuns i in !wrapperfun) in
    OutManyName out_many, List.map2 (fun inp0 wrapper -> {inp=InpName inp0; wrappers=[wrapper]}) inps wrappers

  let create_inp_many : 'var 'v 'c. int -> ('var, 'v list * 'c X.u) constr -> 'c X.t Mergeable.t -> 'v out list * 'var inp_many =
    fun size constr cont ->
    let wrapperfun, wrapper = Wrapper.make constr cont in
    let outs, inp_many0 = Name.create_inp_many size (fun v -> !wrapperfun v) in
    List.map (fun o -> OutName o) outs, {inp_many=GatherName inp_many0; wrappers_many=[wrapper]}

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
       {inp=InpFun(Untyped_dpipeU.(inp_dpipe (flip dpipe) label cont)); wrappers=assert false}
    | Dpipe(_) ->
       assert false
    | Dstream([[dstream]]) ->
       OutFun(Untyped_streamU.out_untyped dstream label),
       {inp=InpFun(Untyped_streamU.(inp_untyped (flip dstream) label cont)); wrappers=assert false}
    | Dstream(_) ->
       assert false

  let create_untyped_out_many env ~from ~to_ label conts =
    let from_info = Env.metainfo env from 1
    and to_info = Env.metainfo env to_ 1
    in
    match generate ~from_info ~to_info with
    | Dpipe([dpipes]) ->
       OutManyFun(List.map (fun dpipe -> Untyped_dpipeU.out_dpipe dpipe label) dpipes),
       List.map2 (fun dpipe cont -> {inp=InpFun(Untyped_dpipeU.(inp_dpipe (flip dpipe) label cont)); wrappers=assert false}) dpipes conts
    | Dpipe(_) ->
       assert false
    | Dstream([dstreams]) ->
       OutManyFun(List.map (fun dstream -> Untyped_streamU.out_untyped dstream label) dstreams),
       List.map2 (fun dstream cont -> {inp=InpFun(Untyped_streamU.(inp_untyped (flip dstream) label cont)); wrappers=assert false}) dstreams conts
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
       {inp_many=GatherFun(Untyped_dpipeU.(inplist_dpipe (List.map flip dpipes) label cont)); wrappers_many=assert false}
    | Dstream(dstreams) ->
       let dstreams = List.map List.hd dstreams in
       List.map (fun dstream -> OutFun(Untyped_streamU.out_untyped dstream label)) dstreams,
       {inp_many=GatherFun(Untyped_streamU.(inplist_untyped (List.map flip dstreams) label cont)); wrappers_many=assert false}

  let merge_out o1 o2 =
    match o1, o2 with
    | OutName o1, OutName o2 ->
       OutName (Name.merge_out o1 o2)
    | OutFun _, OutFun _ ->
       o1
    | OutName _, OutFun _ | OutFun _, OutName _ ->
       assert false

  let merge_inp (i1: 'var inp) (i2: 'var inp) : 'var inp =
    match i1, i2 with
    | {inp=InpName o1; wrappers=w1}, {inp=InpName o2; wrappers=w2} ->
       {inp=InpName (Name.merge_inp o1 o2); wrappers=Wrapper.merge w1 w2}
    | {inp=InpFun (f,wrap1); wrappers=w1}, {inp=InpFun (_, wrap2); wrappers=w2} ->
       {inp=InpFun (f, U.merge_wrappers wrap1 wrap2); wrappers=Wrapper.merge w1 w2}
    | _ ->
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
    | {inp_many=GatherName i1; wrappers_many=w1}, {inp_many=GatherName i2; wrappers_many=w2} ->
       {inp_many=GatherName (Name.merge_inp_many i1 i2); wrappers_many=Wrapper.merge w1 w2}
    | {inp_many=GatherFun i1; wrappers_many=w1}, {inp_many=GatherFun i2; wrappers_many=w2} ->
       let f1, wrap1 = i1
       and _, wrap2 = i2
       in
       {inp_many=GatherFun(f1, U.merge_wrappers wrap1 wrap2); wrappers_many=Wrapper.merge w1 w2}
    | _ ->
       assert false

  let[@inline] send out v =
    match out with
    | OutName out -> Name.send out v
    | OutFun f -> f v

  let[@inline] receive (inp:'var inp) =
    match inp with
    | {inp=InpName inp; _} -> Name.receive inp
    | {inp=InpFun (f, wrappers); _} ->
       IO.bind (f ()) (fun[@inline] (tag,v) ->
       IO.return (U.apply_wrapper wrappers tag v))

  let send_many outs f =
    match outs with
    | OutManyName outs -> Name.send_many outs f
    | OutManyFun outs ->
       IO_list.iteri (fun i out -> out (f i)) outs

  let receive_many (inps: 'var inp_many) =
    match inps with
    | {inp_many=GatherName inps; _} -> Name.receive_many inps
    | {inp_many=GatherFun (fs, wrappers); _} ->
       let* xs = IO_list.map (fun f -> f ()) fs in
       let (tags, vs) = List.split xs in
       match tags with
       | tag::_ ->
          IO.return (U.apply_wrapper wrappers tag vs)
       | [] ->
          assert false
end[@@inline]
