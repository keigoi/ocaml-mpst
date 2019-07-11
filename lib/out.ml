open Base
open Common

module Make(EP:S.ENDPOINTS)(M:S.MONAD)(EV:S.EVENT with type 'a monad = 'a M.t) : sig
  type _ out
  val send : ('v one * 't) out EP.lin -> 'v -> 't M.t
  val sendmany : ('v list * 't) out EP.lin -> (int -> 'v) -> 't M.t
  val create_out_one :
    'v EV.channel list ref list ->
    (< .. > as 'obj, _, ('v one * 't) out EP.lin, _) label -> 't EP.t -> 'obj EP.t
  val create_out_many :
    'v EV.channel list ref list ->
    (< .. > as 'obj, _, ('v list * 't) out EP.lin, _) label -> 't EP.t -> 'obj EP.t

  val create_out_fun_one :
    outfuns:('v -> unit EV.monad) list ->
    (< .. > as 'obj, _, ('v one * 't) out EP.lin, _) label -> 't EP.t -> 'obj EP.t

  val create_out_fun_many :
    outfuns:('v -> unit EV.monad) list list ->
    (< .. > as 'obj, _, ('v list * 't) out EP.lin, _) label -> 't EP.t -> 'obj EP.t
end = struct

  type 'v bare_out =
    | BareOutChan of 'v EV.channel list ref
    | BareOutFun of ('v -> unit EV.monad) list

  type _ out =
    | Out : 'u bare_out * int * 't EP.t -> ('u one * 't) out
    | OutMany : 'u bare_out * int * 't EP.t -> ('u list * 't) out

  let unify a b =
    match a,b with
    | BareOutChan(a), BareOutChan(b) -> a := !b
    | BareOutFun(_), BareOutFun(_) -> ()
    | _, _ -> assert false

  let merge_out : type u t. (u * t) out -> (u * t) out -> (u * t) out =
    fun out1 out2 ->
    match out1, out2 with
    | Out(b1,i1,c1), Out(b2,i2,c2) ->
       assert (i1=i2);
       unify b1 b2;
       Out(b1,i1,EP.make_merge c1 c2)
    | OutMany(b1,i1,c1), OutMany(b2,i2,c2) ->
       assert (i1=i2);
       unify b1 b2;
       OutMany(b1,i1,EP.make_merge c1 c2)

  let create_out_one chss label cont =
    let out = 
      EP.make_lin
        ~hook:(lazy (EP.force_merge cont))
        ~mergefun:merge_out
        ~values:
        (List.mapi (fun i chs ->
             assert (List.length !chs = 1);
             Out(BareOutChan chs,i,cont))
           chss)
    in
    EP.wrap_label label.obj out

  let create_out_many chss label cont =
    let out = 
      EP.make_lin
        ~hook:(lazy (EP.force_merge cont))
        ~mergefun:merge_out
        ~values:
        (List.mapi (fun i chs ->
             OutMany(BareOutChan chs,i,cont))
           chss)
    in
    EP.wrap_label label.obj out

  let create_out_fun_one ~outfuns label cont =
    let out =
      EP.make_lin
        ~hook:(Lazy.from_val ())
        ~mergefun:merge_out
        ~values:
        (List.mapi (fun i f ->
             Out(BareOutFun [f], i, cont)) outfuns)
    in
    EP.wrap_label label.obj out

  let create_out_fun_many ~outfuns label cont =
    let out =
      EP.make_lin
        ~hook:(Lazy.from_val ())
        ~mergefun:merge_out
        ~values:
        (List.mapi (fun i fs ->
             OutMany(BareOutFun fs, i, cont)) outfuns)
    in
    EP.wrap_label label.obj out

  let send t v =
    let Out(ch,i,cont) = EP.use t in
    match ch with
    | BareOutChan chs ->
       (M.bind (EV.sync (EV.send (List.hd !chs) v))) (fun () ->
       M.return (EP.fresh cont i))
    | BareOutFun fs ->
       M.bind (List.hd fs v) (fun () ->
       M.return @@ EP.fresh cont i)

  let sendmany t vf =
    let OutMany(ch,i,cont) = EP.use t in
    match ch with
    | BareOutChan chs ->
       M.bind (M.iteriM (fun i ch -> EV.sync (EV.send ch (vf i))) !chs) (fun () ->
       M.return (EP.fresh cont i))
    | BareOutFun fs ->
       M.bind (M.iteriM (fun i f -> f (vf i)) fs) (fun () ->
       M.return @@ EP.fresh cont i)
end
