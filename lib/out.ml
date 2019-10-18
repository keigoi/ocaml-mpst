open Base
open Common
module Make(EP:S.ENDPOINTS)(M:S.MONAD)(EV:S.EVENT with type 'a monad = 'a M.t) : sig

  type _ bare_out =
    | BareOutChanOne : 'v one EV.out -> 'v bare_out
    | BareOutChanMany : 'v list EV.out -> 'v bare_out
    | BareOutFun : ('v -> unit EV.monad) list -> 'v bare_out
  type _ out

  val make_out :
    (< .. > as 'obj, _, ('v one * 't) out EP.lin, _) label
    -> 'v bare_out list
    -> 't EP.t
    -> 'obj  EP.t
  val make_outmany :
    (< .. > as 'obj, _, ('v list * 't) out EP.lin, _) label
    -> 'v bare_out
    -> 't EP.t
    -> 'obj  EP.t

  val merge_out : ('v * 't) out -> ('v * 't) out -> ('v * 't) out

  val send : ('v one * 't) out EP.lin -> 'v -> 't M.t

  (* val sendmany : ('v list * 't) out EP.lin -> (int -> 'v) -> 't M.t *)
end = struct
  type 'v bare_out =
    | BareOutChanOne : 'v one EV.out -> 'v bare_out
    | BareOutChanMany : 'v list EV.out -> 'v bare_out
    | BareOutFun : ('v -> unit EV.monad) list -> 'v bare_out

  type _ out =
    | Out : int * 'v bare_out * 't EP.t -> ('v one * 't) out
    | OutMany : 'v bare_out * 't EP.t -> ('v list * 't) out

  let merge_bare_out : 'v. 'v bare_out -> 'v bare_out -> 'v bare_out = fun out1 out2 ->
    match out1,out2 with
    | BareOutChanOne(c1),BareOutChanOne(c2) ->
       BareOutChanOne(EV.merge_out c1 c2)
    | BareOutChanMany(c1),BareOutChanMany(c2) ->
       BareOutChanMany(EV.merge_out c1 c2)
    | BareOutFun(_) as f,BareOutFun(_) ->
       f
    | _ ->
       failwith "merge_bare_out: impossible"

  let merge_out : type u t. (u * t) out -> (u * t) out -> (u * t) out =
    fun out1 out2 ->
    match out1, out2 with
    | Out(i1,b1,c1), Out(i2,b2,c2) ->
       assert (i1=i2);
       let b = merge_bare_out b1 b2 in
       Out(i1,b,EP.make_merge c1 c2)
    | OutMany(b1,c1), OutMany(b2,c2) ->
       let b = merge_bare_out b1 b2 in
       OutMany(b,EP.make_merge c1 c2)
       
  let make_out label chs conts =
    let out =
      EP.make_lin
        ~hook:(lazy (EP.force_merge conts))
        ~mergefun:merge_out
        ~values:
        (List.mapi (fun i ch -> Out(i,ch,conts)) chs)
    in
    EP.wrap_label label.obj out
    
  let make_outmany label ch cont = 
    let out =
      EP.make_lin
        ~hook:(lazy (EP.force_merge cont))
        ~mergefun:merge_out
        ~values:
        [OutMany(ch,cont)]
    in
    EP.wrap_label label.obj out

  let[@inline] send t v =
    let Out(i,ch,cont) = EP.use t in
    match ch with
    | BareOutChanOne(ch) ->
       (M.bind (EV.sync (EV.send_st ch v))) (fun[@inline] () ->
       M.return (EP.fresh cont i))
    | BareOutFun fs ->
       M.bind (List.hd fs v) (fun[@inline] () ->
       M.return @@ EP.fresh cont i)

  (* let sendmany t vf =
   *   let Out(i,ch,cont) = EP.use t in
   *   match ch with
   *   | BareOutChan chs ->
   *      M.bind (M.iteriM (fun[@inline] i ch -> EV.sync (EV.send ch (vf i))) !chs) (fun () ->
   *      M.return (EP.fresh cont i))
   *   | BareOutFun fs ->
   *      M.bind (M.iteriM (fun[@inline] i f -> f (vf i)) fs) (fun () ->
   *      M.return @@ EP.fresh cont i) *)
end

(* module Make_(EP:S.ENDPOINTS)(M:S.MONAD)(EV:S.EVENT with type 'a monad = 'a M.t) : sig
 *   type _ out
 *   val send : ('v one * 't) out EP.lin -> 'v -> 't M.t
 *   val sendmany : ('v list * 't) out EP.lin -> (int -> 'v) -> 't M.t
 *   val create_out_one :
 *     'v EV.channel list ref list ->
 *     (< .. > as 'obj, _, ('v one * 't) out EP.lin, _) label -> 't EP.t -> 'obj EP.t
 *   val create_out_many :
 *     'v EV.channel list ref list ->
 *     (< .. > as 'obj, _, ('v list * 't) out EP.lin, _) label -> 't EP.t -> 'obj EP.t
 * 
 *   val create_out_fun_one :
 *     outfuns:('v -> unit EV.monad) list ->
 *     (< .. > as 'obj, _, ('v one * 't) out EP.lin, _) label -> 't EP.t -> 'obj EP.t
 * 
 *   val create_out_fun_many :
 *     outfuns:('v -> unit EV.monad) list list ->
 *     (< .. > as 'obj, _, ('v list * 't) out EP.lin, _) label -> 't EP.t -> 'obj EP.t
 * end = struct
 * 
 *   type 'v bare_out =
 *     | BareOutChan of 'v EV.channel list ref
 *     | BareOutFun of ('v -> unit EV.monad) list
 * 
 *   type _ out =
 *     | Out : 'u bare_out * int * 't EP.t -> ('u one * 't) out
 *     | OutMany : 'u bare_out * int * 't EP.t -> ('u list * 't) out
 * 
 *   let unify a b =
 *     match a,b with
 *     | BareOutChan(a), BareOutChan(b) -> a := !b
 *     | BareOutFun(_), BareOutFun(_) -> ()
 *     | _, _ -> assert false
 * 
 *   let merge_out : type u t. (u * t) out -> (u * t) out -> (u * t) out =
 *     fun out1 out2 ->
 *     match out1, out2 with
 *     | Out(b1,i1,c1), Out(b2,i2,c2) ->
 *        assert (i1=i2);
 *        unify b1 b2;
 *        Out(b1,i1,EP.make_merge c1 c2)
 *     | OutMany(b1,i1,c1), OutMany(b2,i2,c2) ->
 *        assert (i1=i2);
 *        unify b1 b2;
 *        OutMany(b1,i1,EP.make_merge c1 c2)
 * 
 *   let create_out_one chss label cont =
 *     let out =
 *       EP.make_lin
 *         ~hook:(lazy (EP.force_merge cont))
 *         ~mergefun:merge_out
 *         ~values:
 *         (List.mapi (fun i chs ->
 *              assert (List.length !chs = 1);
 *              Out(BareOutChan chs,i,cont))
 *            chss)
 *     in
 *     EP.wrap_label label.obj out
 * 
 *   let create_out_many chss label cont =
 *     let out =
 *       EP.make_lin
 *         ~hook:(lazy (EP.force_merge cont))
 *         ~mergefun:merge_out
 *         ~values:
 *         (List.mapi (fun i chs ->
 *              OutMany(BareOutChan chs,i,cont))
 *            chss)
 *     in
 *     EP.wrap_label label.obj out
 * 
 *   let create_out_fun_one ~outfuns label cont =
 *     let out =
 *       EP.make_lin
 *         ~hook:(Lazy.from_val ())
 *         ~mergefun:merge_out
 *         ~values:
 *         (List.mapi (fun i f ->
 *              Out(BareOutFun [f], i, cont)) outfuns)
 *     in
 *     EP.wrap_label label.obj out
 * 
 *   let create_out_fun_many ~outfuns label cont =
 *     let out =
 *       EP.make_lin
 *         ~hook:(Lazy.from_val ())
 *         ~mergefun:merge_out
 *         ~values:
 *         (List.mapi (fun i fs ->
 *              OutMany(BareOutFun fs, i, cont)) outfuns)
 *     in
 *     EP.wrap_label label.obj out
 * 
 *   let[@inline] send t v =
 *     let Out(ch,i,cont) = EP.use t in
 *     match ch with
 *     | BareOutChan chs ->
 *        (M.bind (EV.sync (EV.send (List.hd !chs) v))) (fun[@inline] () ->
 *        M.return (EP.fresh cont i))
 *     | BareOutFun fs ->
 *        M.bind (List.hd fs v) (fun[@inline] () ->
 *        M.return @@ EP.fresh cont i)
 * 
 *   let sendmany t vf =
 *     let OutMany(ch,i,cont) = EP.use t in
 *     match ch with
 *     | BareOutChan chs ->
 *        M.bind (M.iteriM (fun[@inline] i ch -> EV.sync (EV.send ch (vf i))) !chs) (fun () ->
 *        M.return (EP.fresh cont i))
 *     | BareOutFun fs ->
 *        M.bind (M.iteriM (fun[@inline] i f -> f (vf i)) fs) (fun () ->
 *        M.return @@ EP.fresh cont i)
 * end[@@inlined] *)
