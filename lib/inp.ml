open Base

module Make(EP:S.ENDPOINTS)(M:S.MONAD)(EV:S.EVENT with type 'a monad = 'a M.t) : sig
  type 'a inp

  type 'var inpfun = {
      raw_input : unit -> (tag * Obj.t) M.t;
      cases : (tag * (Obj.t -> 'var)) list;
    }

  val receive : 'a inp EP.lin -> 'a M.t

  val create_inp_one :
    'v EV.channel list ref list ->
    (_,[>] as 'var,_,'v * 't) label -> 't EP.t -> 'var inp EP.lin EP.t

  val create_inp_many :
    'v EV.channel list ref list ->
    (_,[>] as 'var,_,'v list * 't) label -> 't EP.t -> 'var inp EP.lin EP.t

  val create_inp_fun :
    'var inpfun list -> 'var inp EP.lin EP.t

end = struct

  type 'var inpfun = {
      raw_input : unit -> (tag * Obj.t) M.t;
      cases : (tag * (Obj.t -> 'var)) list;
    }

  type 'a inp =
    | InpEv of 'a EV.inp
    | InpFun of 'a inpfun

  let merge_inp ev1 ev2 =
    match ev1, ev2 with
    | InpEv ev1, InpEv ev2 ->
       InpEv (EV.merge_inp ev1 ev2)
    | InpFun f, InpFun g ->
       InpFun {f with cases = f.cases @ g.cases}
    | _, _ ->
       assert false

  let create_inp_fun fs =
    EP.make_lin
      ~hook:(Lazy.from_val ())
      ~mergefun:merge_inp
      ~values:
      (List.map (fun f -> InpFun f) fs)

  let[@inline] create_inp_ ~recvfun chs label cont =
    (* we must guard it -- chs is a placeholder for channels
     * which might be "unified" during merge (see out.ml).
     * if we do not delay, and if the channel unification occurs,
     * input will block indefinitely.
     *)
    EP.make_lin
      ~hook:(lazy (EP.force_merge cont))
      ~mergefun:merge_inp
      ~values:
      (List.init (List.length !(List.hd chs))
         (fun me ->
           InpEv
             (EV.wrap_inp
                (recvfun me)
                (fun[@inline] v -> label.var (v, EP.fresh cont me)))))

  let[@inline] create_inp_one chs label cont =
    let[@inline] receive me =
      assert (List.length chs = 1);
      let chs' = List.hd chs in
      let ch = List.nth !chs' 0 in
      let ch = EV.flip_channel ch in
      EV.inp ch
    in
    create_inp_ receive chs label cont

  let create_inp_many chs label cont =
    let receive_many me =
      (* again, delay this *)
      let chs =
        List.map (fun chs ->
            let ch = List.nth !chs me in
            EV.flip_channel ch)
          chs
      in
      EV.receive_list_inp chs
    in
    create_inp_ receive_many chs label cont

  let[@inline] receive inp =
    match EP.use inp with
    | InpEv ev ->
       EV.sync (EV.receive_inp ev)
    | InpFun f ->
       (* receive tag(s) *)
       M.bind (f.raw_input ()) (fun (tag,v) ->
       let alt = List.assoc tag f.cases in
       M.return (alt v))
end[@@inlined]
