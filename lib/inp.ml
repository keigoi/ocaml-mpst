open Base
module Make(EP:S.ENDPOINTS)(StaticLin:S.LIN)(M:S.MONAD)(EV:S.EVENT with type 'a monad = 'a M.t) : sig
  type 'a inp

  type raw_input_fun = unit -> (tag * Obj.t) M.t

  val make_inpfun :
    (_, 'var, _, 'v * 'cont StaticLin.lin) label
    -> tag * raw_input_fun list
    -> 'cont EP.t
    -> 'var inp EP.lin EP.t

  val make_inpfunmany :
    (_, 'var, _, 'v list * 'cont StaticLin.lin) label
    -> tag * raw_input_fun list
    -> 'cont EP.t
    -> 'var inp EP.lin EP.t

  val make_inp : 'var one EV.inp list -> 'var inp EP.lin EP.t

  val make_inpmany : 'var list EV.inp -> 'var inp EP.lin EP.t

  val receive : 'a inp -> 'a M.t

end = struct

  type raw_input_fun = unit -> (tag * Obj.t) M.t
  type ('v,'var) varfun =
    VarFun : 'cont EP.t * ('v * 'cont EP.t -> 'var) -> ('v,'var) varfun

  type ('v,'var) inpfun = {
      raw_input_fun : unit -> (tag * 'v) M.t;
      wrappers : (tag * ('v,'var) varfun) list;
    }

  type 'a inp =
    | InpChanOne of 'a one EV.inp
    | InpChanMany of 'a list EV.inp
    | InpFun of (Obj.t,'a) inpfun
    | InpFunMany of (Obj.t list,'a) inpfun

  let merge_inp inp1 inp2 =
    match inp1, inp2 with
    | InpChanOne inp1, InpChanOne inp2 ->
       InpChanOne (EV.merge_inp inp1 inp2)
    | InpChanMany inp1, InpChanMany inp2 ->
       InpChanMany (EV.merge_inp inp1 inp2)
    | InpFun f, InpFun g ->
       InpFun {f with wrappers = f.wrappers @ g.wrappers}(*TODO merge continuation*)
    | _, _ -> assert false

  let[@inline] make_inpfun label (tag,fs) conts =
    EP.make_lin
      ~hook:(Lazy.from_val ()) (* FIXME force continuation *)
      ~mergefun:merge_inp
      ~values:
      (List.mapi
         (fun[@inline] idx f ->
           let inpfun =
             {raw_input_fun = f;
              wrappers=
                [(tag,
                  VarFun (conts, 
                          (fun[@inline] (v,conts) ->
                            label.var (Obj.obj v, StaticLin.create_dummy @@ EP.fresh conts idx))))]
             }
           in
           InpFun(inpfun))
         fs)

  let[@inline] receive_list fs () =
    M.bind
      (List.hd fs ())
      (fun (tag,v) ->
        M.bind
          (M.mapM (fun f -> f ()) (List.tl fs))
          (fun vs ->
            let vs = List.map snd vs in
            M.return (tag, List.map Obj.repr vs)))

  let[@inline] make_inpfunmany label (tag,fs) conts =
    let inpfun =
      {raw_input_fun = receive_list fs;
       wrappers=
         [(tag,
           VarFun (conts, 
                   (fun (v,conts) ->
                     label.var (List.map Obj.obj v, StaticLin.create_dummy @@ EP.fresh conts 0))))]
      }
    in
    EP.make_lin
      ~hook:(Lazy.from_val ()) (* FIXME force continuation *)
      ~mergefun:merge_inp
      ~values:[InpFunMany(inpfun)]

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
                       
  let[@inline] receive = function
    | InpChanOne inp ->
       EV.sync (EV.receive_st inp)
    | InpChanMany inp ->
       EV.sync (EV.receivemany_st inp)
    | InpFun f ->
       (* receive tag(s) *)
       M.bind (f.raw_input_fun ()) (fun[@inline] (tag,v) ->
       let (VarFun(cont,f)) = List.assoc tag f.wrappers in
       M.return (f (v,cont)))
    | InpFunMany f ->
       (* receive tag(s) *)
       M.bind (f.raw_input_fun ()) (fun[@inline] (tag,v) ->
       let (VarFun(cont,f)) = List.assoc tag f.wrappers in
       M.return (f (v,cont)))
end[@@inlined]
