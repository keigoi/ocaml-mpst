open Base
open Common
module Make(EP:S.ENDPOINTS)(M:S.MONAD)(EV:S.EVENT with type 'a monad = 'a M.t) : sig

  type _ bare_out =
    | BareOutChanOne : 'v one EV.out -> 'v bare_out
    | BareOutChanMany : 'v list EV.out -> 'v bare_out
    | BareOutFun : ('v -> unit EV.monad) list -> 'v bare_out
  type _ out

  val make_out_single :
    (< .. > as 'obj, _, ('v one * 't) out EP.lin, _) label
    -> 'v bare_out
    -> 't EP.t
    -> 'obj EP.t

  val make_out :
    (< .. > as 'obj, _, ('v one * 't) out EP.lin, _) label
    -> 'v bare_out list
    -> 't EP.t
    -> 'obj EP.t
  val make_outmany :
    (< .. > as 'obj, _, ('v list * 't) out EP.lin, _) label
    -> 'v bare_out
    -> 't EP.t
    -> 'obj EP.t

  val merge_out : ('v * 't) out -> ('v * 't) out -> ('v * 't) out

  val send : ('v one * 't) out EP.lin -> 'v -> 't M.t

  val sendmany : ('v list * 't) out EP.lin -> (int -> 'v) -> 't M.t
end = struct
  type 'v bare_out =
    | BareOutChanOne : 'v one EV.out -> 'v bare_out
    | BareOutChanMany : 'v list EV.out -> 'v bare_out
    | BareOutFun : ('v -> unit EV.monad) list -> 'v bare_out

  type _ out =
    | Out : 'v bare_out * int * 't EP.t -> ('v one * 't) out
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
    | Out(b1,i1,c1), Out(b2,i2,c2) ->
       assert (i1=i2);
       let b = merge_bare_out b1 b2 in
       Out(b,i1,EP.make_merge c1 c2)
    | OutMany(b1,c1), OutMany(b2,c2) ->
       let b = merge_bare_out b1 b2 in
       OutMany(b,EP.make_merge c1 c2)
       
  let make_out label chs conts =
    let out =
      EP.make_lin
        ~hook:(lazy (EP.force_merge conts))
        ~mergefun:merge_out
        ~values:
        (List.mapi (fun i ch -> Out(ch,i,conts)) chs)
    in
    EP.wrap_label label.obj out
       
  let make_out_single label ch conts =
    make_out label [ch] conts
    
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
    let Out(ch,i,cont) = EP.use t in
    match ch with
    | BareOutChanOne(ch) ->
       (M.bind (EV.sync (EV.send_st ch v))) (fun[@inline] () ->
           M.return (EP.fresh cont i))
    | BareOutFun fs ->
       assert (List.length fs = 1);
       M.bind (List.hd fs v) (fun[@inline] () ->
       M.return @@ EP.fresh cont i)
    | BareOutChanMany(_) ->
       assert false

  let[@inline] sendmany t vf =
    let OutMany(ch,cont) = EP.use t in
    match ch with
    | BareOutChanMany(ch) ->
       (M.bind (EV.sync (EV.sendmany_st ch vf))) (fun[@inline] () ->
       M.return (EP.fresh cont 0))
    | BareOutFun fs ->
       M.bind (M.iteriM (fun i f -> f (vf i)) fs) (fun[@inline] () ->
       M.return @@ EP.fresh cont 0)
    | BareOutChanOne(_) ->
       assert false
end[@@inlined]
