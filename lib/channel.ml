open Base
open Common

module Make
         (EP:S.ENDPOINTS)
         (StaticLin:S.LIN)
         (M:S.MONAD)
         (EV:S.EVENT with type 'a monad = 'a M.t)
         (C:S.SERIAL with type 'a monad = 'a M.t)
  = struct
  include Global_common.Make(EP)
  module Out = Out.Make(EP)(M)(EV)
  module Inp = Inp.Make(EP)(StaticLin)(M)(EV)
  module Dpipe = Make_dpipe(C)

  (* open Out
   * open Inp
   * open Dpipe *)

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

  let generate ~from_info ~to_info =
    match from_info.rm_kind, to_info.rm_kind with
    | EpLocal, EpLocal ->
       failwith ""
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

  let make_wrapper_untyped label cont idx =
    let tag = make_tag label.var in
    [(tag, (fun t -> label.var (Obj.obj t, StaticLin.create_dummy @@ EP.fresh cont idx)))]

  let wrap_dpipe label chs conts =
    let tag = make_tag label.var in
    let makefun ch = (fun () -> C.input_tagged ch.Dpipe.me.inp) in
    Inp.make_inpfun
      label
      (tag, List.map makefun chs)
      conts
    
  let wrap_untyped label chs conts =
    let tag = make_tag label.var in
    let makefun ch = (fun () -> EV.sync (EV.receive ch)) in
    Inp.make_inpfun
      label
      (tag, List.map makefun chs)
      conts

  let outfun_dpipe label chss =
    let tag = make_tag label.var in
    let make ch =
      fun v ->
      M.bind (C.output_tagged ch.Dpipe.me.out (tag, Obj.repr v))
        (fun () -> C.flush ch.me.out)
    in
    List.map (fun chs -> Out.BareOutFun(List.map make chs)) chss

  let outfun_untyped label chss =
    let tag = make_tag label.var in
    let make ch =
      fun v -> EV.sync (EV.send ch (tag, Obj.repr v))
    in
    List.map (fun chs -> Out.BareOutFun(List.map make chs)) chss
    
  let generate_one label conts_to from_info to_info =
    let chss = generate ~from_info ~to_info in
    match chss with
    | ChVec(ch) ->
       let out, inp =
         EV.wrap ch
           (fun v -> label.var (v, StaticLin.create_dummy  @@ EP.fresh conts_to 0))
       in
       ([Out.BareOutChanOne(out)],
        Inp.make_inp [inp])
    | Untyped(chss) ->
       let chss' = flip_all EV.flip_channel chss in
       (outfun_untyped label chss,
        wrap_untyped label (List.hd chss') conts_to)
    | Dpipe(chss) ->
       let chss' = flip_all Dpipe.flip_dpipe chss in
       (outfun_dpipe label chss,
        wrap_dpipe label (List.hd chss') conts_to)

  let generate_scatter label conts_to from_info to_info =
    let chss = generate ~from_info ~to_info in
    match chss with
    | ChVec(ch) ->
       let out,inp =
         EV.wrap_scatter ch
           (fun v -> label.var (v, StaticLin.create_dummy @@ EP.fresh conts_to 0))
       in
       (Out.BareOutChanMany(out),
        Inp.make_inp inp)
    | Untyped(chss) ->
       let chss' = flip_all EV.flip_channel chss in
       (List.hd (outfun_untyped label chss),
        wrap_untyped label (List.map List.hd chss') conts_to)
    | Dpipe(chss) ->
       let chss' = flip_all Dpipe.flip_dpipe chss in
       (List.hd @@ outfun_dpipe label chss,
        wrap_dpipe label (List.map List.hd chss') conts_to)

  let generate_gather label conts_to from_info to_info =
    let chss = generate ~from_info ~to_info in
    match chss with
    | ChVec(ch) ->
       let out,inp =
         EV.wrap_gather ch
           (fun v -> label.var (v, StaticLin.create_dummy @@ EP.fresh conts_to 0))
       in
       (List.map (fun v -> Out.BareOutChanOne(v)) out,
        Inp.make_inpmany inp)
    | Untyped(chss) ->
       let chss' = flip_all EV.flip_channel chss in
       (outfun_untyped label chss,
        failwith "")
        (* wrap_untyped label (List.hd chss') conts_to) *)
    | Dpipe(chss) ->
       let chss' = flip_all Dpipe.flip_dpipe chss in
       (outfun_dpipe label chss,
        failwith "")
        (* wrap_dpipe label (List.hd chss') conts_to) *)
end
