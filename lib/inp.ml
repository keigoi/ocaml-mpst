open Base
module Make(EP:S.ENDPOINTS)(StaticLin:S.LIN)(M:S.MONAD)(EV:S.EVENT with type 'a monad = 'a M.t) : sig
  type 'a inp

  val make_inp : 'var one EV.inp list -> 'var inp EP.lin EP.t

  val make_inpmany : 'var list EV.inp -> 'var inp EP.lin EP.t

  type untyped_chan = (tag * Obj.t) EV.channel

  val make_inp_untyped :
    (_, [>] as 'var, _, 'v * 'cont StaticLin.lin) label
    -> untyped_chan list
    -> 'cont EP.t
    -> 'var inp EP.lin EP.t

  val make_inpmany_untyped :
    (_, [>] as 'var, _, 'v list * 'cont StaticLin.lin) label
    -> untyped_chan list
    -> 'cont EP.t
    -> 'var inp EP.lin EP.t

  type 'v raw_input_fun = unit -> (tag * 'v) M.t

  val make_inpfun :
    (_, [>] as 'var, _, 'v * 'cont StaticLin.lin) label
    -> Obj.t raw_input_fun list
    -> 'cont EP.t
    -> 'var inp EP.lin EP.t

  val make_inpfunmany :
    (_, [>] as 'var, _, 'v list * 'cont StaticLin.lin) label
    -> Obj.t raw_input_fun list
    -> 'cont EP.t
    -> 'var inp EP.lin EP.t

  val receive : 'a inp -> 'a M.t

end = struct

  type ('v,'var) wrapper =
    Wrapper :
      'cont EP.t * ('v * 'cont EP.t -> 'var) -> ('v,'var) wrapper

  type ('v,'var) wrappers =
    (tag * ('v,'var) wrapper) list

  type untyped_chan = (tag * Obj.t) EV.channel
  type 'v raw_input_fun = unit -> (tag * 'v) M.t

  type 'a inp =
    | InpChanOne of 'a one EV.inp
    | InpChanMany of 'a list EV.inp
    | InpUntypedOne of untyped_chan * (Obj.t,'a) wrappers
    | InpUntypedMany of untyped_chan list * (Obj.t list,'a) wrappers
    | InpFun of Obj.t raw_input_fun * (Obj.t,'a) wrappers
    | InpFunMany of Obj.t list raw_input_fun * (Obj.t list,'a) wrappers

  (* untyped operation; not for channel vectors *)
  let ep_magic (cont:'t EP.t) : 'u EP.t =
    Obj.magic cont

  let merge_wrapper (tagf,Wrapper(contf,wf)) (tagg,Wrapper(contg,wg)) =
    assert (tagf=tagg);
    (* if polyvar tags are same, we can safely merge them *)
    let cont = EP.make_merge contf (ep_magic contg) in
    (tagf,Wrapper(cont,wf))

  let merge_wrappers fs gs =
    let fdup,fonly =
      List.partition (fun (tagf,Wrapper(_,_)) ->
          List.exists (fun (tagg,Wrapper(_,_)) -> tagf=tagg) gs
        ) fs
    in
    let gdup,gonly =
      List.partition (fun (tagg,_) ->
          List.exists (fun (tagf,_) -> tagf=tagg) fs
        ) gs
    in
    fonly @ gonly @ List.map2 merge_wrapper fdup gdup

  let merge_inp inp1 inp2 =
    match inp1, inp2 with
    | InpChanOne inp1, InpChanOne inp2 ->
       InpChanOne (EV.merge_inp inp1 inp2)
    | InpChanMany inp1, InpChanMany inp2 ->
       InpChanMany (EV.merge_inp inp1 inp2)
    | InpFun(i,f), InpFun(_,g) ->
       InpFun (i, merge_wrappers f g)
    | _, _ -> assert false

  let[@inline] make_inp is =
    EP.make_lin
      ~hook:(Lazy.from_val ())
      ~mergefun:merge_inp
      ~values:(List.map (fun[@inline] i -> InpChanOne(i)) is)

  let[@inline] make_inpmany inp =
    EP.make_lin
      ~hook:(Lazy.from_val ())
      ~mergefun:merge_inp
      ~values:[InpChanMany(inp)]

  let[@inline] wrapper label conts idx =
    (fun[@inline] (v,conts) ->
      (* only for untyped communication; chvec never come here *)
      label.var (Obj.obj v, StaticLin.create_dummy @@ EP.fresh conts idx))

  let[@inline] wrapper_many label conts =
    (fun[@inline] (v,conts) ->
      (* only for untyped communication; chvec never come here *)
      label.var (List.map Obj.obj v, StaticLin.create_dummy @@ EP.fresh conts 0))

  let[@inline] untypedchans label conts idx ch =
    let tag = make_tag label.var in
    InpUntypedOne(ch,[(tag, Wrapper(conts, wrapper label conts idx))])

  let[@inline] untypedmany label conts chs =
    let tag = make_tag label.var in
    InpUntypedMany(chs,[(tag, Wrapper(conts, wrapper_many label conts))])

  let[@inline] make_inp_untyped label chs conts =
    EP.make_lin
      ~hook:(Lazy.from_val ()) (* FIXME force continuation *)
      ~mergefun:merge_inp
      ~values:(List.mapi (untypedchans label conts) chs)

  let[@inline] make_inpmany_untyped label chs conts =
    EP.make_lin
      ~hook:(Lazy.from_val ()) (* FIXME force continuation *)
      ~mergefun:merge_inp
      ~values:(List.mapi (untypedchans label conts) chs)

  let[@inline] inpfuns label conts idx f =
    let tag = make_tag label.var in
    InpFun(f,[(tag, Wrapper (conts, wrapper label conts idx))])

  let[@inline] make_inpfun label fs conts =
    EP.make_lin
      ~hook:(Lazy.from_val ()) (* FIXME force continuation *)
      ~mergefun:merge_inp
      ~values:(List.mapi (inpfuns label conts) fs)

  let[@inline] receive_list fs () =
    M.bind (List.hd fs ()) @@ fun[@inline] (tag,v) ->
    M.bind (M.mapM (fun[@inline] f -> f ()) (List.tl fs)) @@ fun[@inline] vs ->
    let vs = List.map snd vs in
    M.return (tag, List.map Obj.repr (v::vs))

  let[@inline] inpfunmany label fs conts =
    let tag = make_tag label.var in
    InpFunMany(receive_list fs,[(tag, Wrapper (conts, wrapper_many label conts))])

  let[@inline] make_inpfunmany label fs conts =
    EP.make_lin
      ~hook:(Lazy.from_val ()) (* FIXME force continuation *)
      ~mergefun:merge_inp
      ~values:[inpfunmany label fs conts]


  let[@inline] apply_wrapper wrappers tag v =
    let (Wrapper(cont,f)) =
      match wrappers with
      | [wrapper] -> snd wrapper
      | _ -> List.assoc tag wrappers
    in
    f (v,cont)

  let[@inline] receive_chan_list cs =
    M.bind (EV.sync (EV.receive (List.hd cs))) @@ fun[@inline] (tag,v) ->
    M.bind (M.mapM (fun[@inline] c -> EV.sync (EV.receive c)) (List.tl cs)) @@ fun[@inline] vs ->
    let vs = List.map snd vs in
    M.return (tag, List.map Obj.repr (v::vs))

  let[@inline] receive = function
    | InpChanOne inp ->
       EV.sync (EV.receive_st inp)
    | InpChanMany inp ->
       EV.sync (EV.receivemany_st inp)
    | InpUntypedOne(c,ws) ->
       EV.sync @@
         EV.receive_wrap
           c
           (fun[@inline] (tag,v) -> apply_wrapper ws tag v)
    | InpUntypedMany(cs,ws) ->
       M.bind (receive_chan_list cs) @@ fun[@inline] (tag,vs) ->
       M.return (apply_wrapper ws tag vs)
    | InpFun (f,ws) ->
       M.bind (f ()) @@ fun[@inline] (tag,v) ->
       M.return (apply_wrapper ws tag v)
    | InpFunMany (f,ws) ->
       M.bind (f ()) @@ fun[@inline] (tag,v) ->
       M.return (apply_wrapper ws tag v)
end[@@inlined]
