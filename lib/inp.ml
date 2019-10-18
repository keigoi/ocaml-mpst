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
  type 'var varfun =
    VarFun : 'cont EP.t * (Obj.t * 'cont EP.t -> 'var) -> 'var varfun

  type 'var inpfun = {
      raw_input_fun : unit -> (tag * Obj.t) M.t;
      wrappers : (tag * 'var varfun) list;
    }

  type 'a inp =
    | InpChanOne of 'a one EV.inp
    | InpChanMany of 'a list EV.inp
    | InpFun of 'a inpfun

  let merge_inp inp1 inp2 =
    match inp1, inp2 with
    | InpChanOne inp1, InpChanOne inp2 ->
       InpChanOne (EV.merge_inp inp1 inp2)
    | InpChanMany inp1, InpChanMany inp2 ->
       InpChanMany (EV.merge_inp inp1 inp2)
    | InpFun f, InpFun g ->
       InpFun {f with wrappers = f.wrappers @ g.wrappers}(*TODO merge continuation*)
    | _, _ -> assert false

  let make_inpfun label (tag,fs) conts =
    EP.make_lin
      ~hook:(Lazy.from_val ()) (* FIXME force continuation *)
      ~mergefun:merge_inp
      ~values:
      (List.mapi
         (fun idx f ->
           let inpfun =
             {raw_input_fun = f;
              wrappers=
                [(tag,
                  VarFun (conts, 
                          (fun (v,conts) ->
                            label.var (Obj.obj v, StaticLin.create_dummy @@ EP.fresh conts idx))))]
             }
           in
           InpFun(inpfun))
         fs)

  let make_inpfun label (tag,fs) conts =
    EP.make_lin
      ~hook:(Lazy.from_val ()) (* FIXME force continuation *)
      ~mergefun:merge_inp
      ~values:
      (List.mapi
         (fun idx f ->
           let inpfun =
             {raw_input_fun = f;
              wrappers=
                [(tag,
                  VarFun (conts, 
                          (fun (v,conts) ->
                            label.var (Obj.obj v, StaticLin.create_dummy @@ EP.fresh conts idx))))]
             }
           in
           InpFun(inpfun))
         fs)

  let make_inp is =
    EP.make_lin
      ~hook:(Lazy.from_val ())
      ~mergefun:merge_inp
      ~values:(List.map (fun i -> InpChanOne(i)) is)
    
  let make_inpmany inp =
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
       M.bind (f.raw_input_fun ()) (fun (tag,v) ->
       let (VarFun(cont,f)) = List.assoc tag f.wrappers in
       M.return (f (v,cont)))
end[@@inlined]

(* module Make(EP:S.ENDPOINTS)(M:S.MONAD)(EV:S.EVENT with type 'a monad = 'a M.t) : sig
 *   type 'a inp
 * 
 *   type 'var inpfun = {
 *       raw_input : unit -> (tag * Obj.t) M.t;
 *       cases : (tag * (Obj.t -> 'var)) list;
 *     }
 * 
 *   val receive : 'a inp EP.lin -> 'a M.t
 * 
 *   val create_inp_one :
 *     'v EV.channel list ref list ->
 *     (_,[>] as 'var,_,'v * 't) label -> 't EP.t -> 'var inp EP.lin EP.t
 * 
 *   val create_inp_many :
 *     'v EV.channel list ref list ->
 *     (_,[>] as 'var,_,'v list * 't) label -> 't EP.t -> 'var inp EP.lin EP.t
 * 
 *   val create_inp_fun :
 *     'var inpfun list -> 'var inp EP.lin EP.t
 * 
 * end = struct
 * 
 *   type 'var inpfun = {
 *       raw_input : unit -> (tag * Obj.t) M.t;
 *       cases : (tag * (Obj.t -> 'var)) list;
 *     }
 * 
 *   type 'a inp =
 *     | InpEv of 'a EV.inp
 *     | InpFun of 'a inpfun
 * 
 *   let merge_inp ev1 ev2 =
 *     match ev1, ev2 with
 *     | InpEv ev1, InpEv ev2 ->
 *        InpEv (EV.merge_inp ev1 ev2)
 *     | InpFun f, InpFun g ->
 *        InpFun {f with cases = f.cases @ g.cases}
 *     | _, _ ->
 *        assert false
 * 
 *   let create_inp_fun fs =
 *     EP.make_lin
 *       ~hook:(Lazy.from_val ())
 *       ~mergefun:merge_inp
 *       ~values:
 *       (List.map (fun f -> InpFun f) fs)
 * 
 *   let[@inline] create_inp_ ~recvfun chs label cont =
 *     (\* we must guard it -- chs is a placeholder for channels
 *      * which might be "unified" during merge (see out.ml).
 *      * if we do not delay, and if the channel unification occurs,
 *      * input will block indefinitely.
 *      *\)
 *     EP.make_lin
 *       ~hook:(lazy (EP.force_merge cont))
 *       ~mergefun:merge_inp
 *       ~values:
 *       (List.init (List.length !(List.hd chs))
 *          (fun me ->
 *            InpEv
 *              (EV.wrap_inp
 *                 (recvfun me)
 *                 (fun[@inline] v -> label.var (v, EP.fresh cont me)))))
 * 
 *   let[@inline] create_inp_one chs label cont =
 *     let[@inline] receive me =
 *       assert (List.length chs = 1);
 *       let chs' = List.hd chs in
 *       let ch = List.nth !chs' me in
 *       let ch = EV.flip_channel ch in
 *       EV.inp ch
 *     in
 *     create_inp_ receive chs label cont
 * 
 *   let create_inp_many chs label cont =
 *     let receive_many me =
 *       (\* again, delay this *\)
 *       let chs =
 *         List.map (fun chs ->
 *             let ch = List.nth !chs me in
 *             EV.flip_channel ch)
 *           chs
 *       in
 *       EV.receive_list_inp chs
 *     in
 *     create_inp_ receive_many chs label cont
 * 
 *   let[@inline] receive inp =
 *     match EP.use inp with
 *     | InpEv ev ->
 *        EV.sync (EV.receive_inp ev)
 *     | InpFun f ->
 *        (\* receive tag(s) *\)
 *        M.bind (f.raw_input ()) (fun (tag,v) ->
 *        let alt = List.assoc tag f.cases in
 *        M.return (alt v))
 * end[@@inlined] *)
