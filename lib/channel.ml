open Base
open Common

module Make
         (EP:S.ENDPOINTS)
         (StaticLin:S.LIN)
         (M:S.MONAD)
         (EV:S.EVENT with type 'a monad = 'a M.t)
         (C:S.SERIAL with type 'a monad = 'a M.t)
  = struct

  module Out = Out.Make(EP)(M)(EV)
  module Inp = Inp.Make(EP)(StaticLin)(M)(EV)
  module Close = Close.Make(M)
  module Dpipe = Make_dpipe(M)(C)

  type epkind =
    EpLocal
  | EpDpipe of Dpipe.dpipe list Table.t list
  | EpUntyped of (Base.tag * Obj.t) EV.channel list Table.t list

  type 'v chan =
    | ChVec of 'v EV.st
    | Untyped of (tag * Obj.t) EV.channel list list
    | Dpipe of Dpipe.dpipe list list

  let dpipe_table = function
    | EpDpipe ts -> Some ts
    | _ -> None

  let untyped_table = function
    | EpUntyped ts -> Some ts
    | _ -> None

  let flip_all flip chss =
    List.map (List.map flip) @@ transpose chss

  let update_tables tables dest chss =
    List.iter2
      (fun t ch -> Table.put t dest ch)
      tables
      chss

  let get_or_add_channels
        ~newch
        src_tables
        src_role
        other_role
        other_count =
    let make_func () =
      List.init other_count (fun _ -> newch ())
    in
    List.map
      (fun t -> Table.get_or_add t other_role make_func)
      src_tables

  let generate_channels
        ~newch ~flipch ~tablefun ~from_info ~to_info =
    match tablefun from_info.rm_kind, tablefun to_info.rm_kind with
    | Some from_table, to_table_opt ->
       let chss =
         get_or_add_channels
           ~newch
           from_table
           from_info.rm_index
           to_info.rm_index
           to_info.rm_size
       in
       begin match to_table_opt with
       | Some to_table ->
          update_tables
            to_table
            from_info.rm_index
            (flip_all flipch chss)
       | None -> ()
       end;
       chss
    | None, Some table ->
       let chss =
         get_or_add_channels
           ~newch
           table
           to_info.rm_index
           from_info.rm_index
           from_info.rm_size
       in
       flip_all flipch chss
    | _ -> assert false

  let untyped x = Untyped(x)
  let dpipe x = Dpipe(x)

  let[@inline] generate ~from_info ~to_info =
    match from_info.rm_kind, to_info.rm_kind with
    | EpLocal, EpLocal ->
       ChVec(EV.create_st (max from_info.rm_size to_info.rm_size))
    | EpDpipe _, _ | _, EpDpipe _ ->
       dpipe @@
         generate_channels
           ~newch:Dpipe.new_dpipe ~flipch:Dpipe.flip_dpipe ~tablefun:dpipe_table
           ~from_info ~to_info
    | EpUntyped _, _ | _, EpUntyped _->
       untyped @@
         generate_channels
           ~newch:EV.new_channel ~flipch:EV.flip_channel ~tablefun:untyped_table
           ~from_info ~to_info

  let receive_dpipe ch () =
    C.input_tagged ch.Dpipe.me.inp
    
  let receive_untyped ch () =
      EV.sync (EV.receive ch)

  let send_dpipe ch tag v =
    M.bind (C.output_tagged ch.Dpipe.me.out (tag, Obj.repr v))
      (fun () -> C.flush ch.me.out)
    
  let send_untyped ch tag v =
    EV.sync (EV.send ch (tag, Obj.repr v))

  let wrap_one label f conts =
    let tag = make_tag label.var in
    Inp.make_inpfun
      label
      (tag, [f])
      conts

  let wrap_ones label fs conts =
    let tag = make_tag label.var in
    Inp.make_inpfun
      label
      (tag, fs)
      conts

  let wrap_many label fs conts =
    let tag = make_tag label.var in
    Inp.make_inpfunmany
      label
      (tag, fs)
      conts

  let bareout_one label f =
    let tag = make_tag label.var in
    Out.BareOutFun([f tag])

  let bareout_ones label fs =
    let tag = make_tag label.var in
    List.map (fun f -> Out.BareOutFun([f tag])) fs

  let bareout_many label fs =
    let tag = make_tag label.var in
    Out.BareOutFun(List.map (fun f -> f tag) fs)

  let generate_one label conts_to from_info to_info =
    let chss = generate ~from_info ~to_info in
    match chss with
    | ChVec(ch) ->
       let out, inp =
         EV.wrap ch
           (fun[@inline] v -> label.var (v, StaticLin.create_dummy  @@ EP.fresh conts_to 0))
       in
       (Out.BareOutChanOne(out),
        Inp.make_inp [inp])
    | Untyped(chss) ->
       let chss' = flip_all EV.flip_channel chss in
       (bareout_one label (send_untyped @@ List.hd @@ List.hd chss),
        wrap_one label (receive_untyped @@ List.hd @@ List.hd chss') conts_to)
    | Dpipe(chss) ->
       let chss' = flip_all Dpipe.flip_dpipe chss in
       (bareout_one label (send_dpipe @@ List.hd @@ List.hd chss),
        wrap_one label (receive_dpipe @@ List.hd @@ List.hd chss') conts_to)

  let generate_scatter label conts_to from_info to_info =
    let chss = generate ~from_info ~to_info in
    match chss with
    | ChVec(ch) ->
       let out,inp =
         EV.wrap_scatter ch
           (fun[@inline] i v -> label.var (v, StaticLin.create_dummy @@ EP.fresh conts_to i))
       in
       (Out.BareOutChanMany(out),
        Inp.make_inp inp)
    | Untyped(chss) ->
       let chs = List.hd chss in
       let chs' = List.map List.hd @@ flip_all EV.flip_channel chss in
       (bareout_many label (List.map send_untyped chs),
        wrap_ones label (List.map receive_untyped chs') conts_to)
    | Dpipe(chss) ->
       let chs = List.hd chss in
       let chs' = List.map List.hd @@ flip_all Dpipe.flip_dpipe chss in
       (bareout_many label (List.map send_dpipe chs),
        wrap_ones label (List.map receive_dpipe chs') conts_to)

  let generate_gather label conts_to from_info to_info =
    let chss = generate ~from_info ~to_info in
    match chss with
    | ChVec(ch) ->
       let outs,inp =
         EV.wrap_gather ch
           (fun v -> label.var (v, StaticLin.create_dummy @@ EP.fresh conts_to 0))
       in
       (List.map (fun v -> Out.BareOutChanOne(v)) outs,
        Inp.make_inpmany inp)
    | Untyped(chss) ->
       let chs = List.map List.hd chss in
       let chs' = List.hd @@ flip_all EV.flip_channel chss in
       (bareout_ones label (List.map send_untyped chs),
        wrap_many label (List.map receive_untyped chs') conts_to)
    | Dpipe(chss) ->
       let chs = List.map List.hd chss in
       let chs' = List.hd @@ flip_all Dpipe.flip_dpipe chss in
       (bareout_ones label (List.map send_dpipe chs),
        wrap_many label (List.map receive_dpipe chs') conts_to)
end
