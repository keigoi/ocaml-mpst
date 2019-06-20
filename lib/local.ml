open Base
type 'a mrg = int * 'a Mergeable.t

type tag = Tag of Obj.t
let mktag : 'v. ('v -> [>]) -> tag = fun f ->
  Tag (Obj.repr (f (Obj.magic ())))

type pipe = {inp: in_channel; out: out_channel}

let new_pipe () =
  let inp, out = Unix.pipe () in
  {inp=Unix.in_channel_of_descr inp; out=Unix.out_channel_of_descr out}

let pipe_send : 'v. pipe -> tag * 'v -> unit =
  fun {out; _} (tag,v) ->
  output_value out tag;
  output_value out (Obj.repr v);
  flush out

type 'a inp =
  | Ev of LinFlag.t * 'a Event.event
  | PipeIn of LinFlag.t * pipe list * (tag * (pipe list -> 'a)) list

let bare_receive_one inp alt =
  let tag = input_value inp in
  let f = List.assoc tag alt in
  f inp

          
let merge_in ev1 ev2 =
  match ev1, ev2 with
  | Ev (o1, ev1), Ev (o2, ev2) ->
     LinFlag.use o1;
     LinFlag.use o2;
     Ev (LinFlag.create (), Event.choose [ev1; ev2])
  | PipeIn (o1, ps1, alts1), PipeIn (o2, ps2, alts2) ->
     LinFlag.use o1;
     LinFlag.use o2;
     assert (ps1=ps2);
     PipeIn (LinFlag.create (), ps1, alts1 @ alts2)
  | _, _ ->
     assert false (* this won't happen since external choice is directed *)

let receive = function
  | Ev (once,ev) ->
     LinFlag.use once;
     Event.sync ev
  | PipeIn (once,ps,alts) ->
     let ts = List.map (fun {inp;_} -> input_value inp) ps in
     let t = List.hd ts in
     let f = List.assoc t alts in
     f ps

type 'v bareout =
  BareOutChan of 'v Event.channel list ref
| BareOutIPC of tag * pipe list

let unify a b =
  match a,b with
  | BareOutChan(a), BareOutChan(b) -> a := !b
  | BareOutIPC(_), BareOutIPC(_) -> ()
  | _, _ -> assert false

let size = function
  | BareOutChan(xs) -> List.length !xs
  | BareOutIPC(_,xs) -> List.length xs
           
type _ out =
  | Out : LinFlag.t * 'u bareout * 't mrg -> ('u one * 't) out
  | OutMany : LinFlag.t * 'u bareout * 't mrg -> ('u list * 't) out

let baresendone ch v =
  match ch with
  | BareOutChan chs -> Event.sync (Event.send (List.hd !chs) v)
  | BareOutIPC (tag,chs) -> pipe_send (List.hd chs) (tag,v)

let baresendmany ch vf =
  match ch with
  | BareOutChan chs -> List.iteri (fun i ch -> Event.sync (Event.send ch (vf i))) !chs
  | BareOutIPC (tag,chs) -> List.iteri (fun i ch -> pipe_send ch (tag, vf i)) chs
                       
let merge_out : type u t. (u * t) out -> (u * t) out -> (u * t) out =
  fun out1 out2 ->
  let mergelocal  (o1,s1,(i1,c1)) (o2,s2,(i2,c2)) =
    assert (i1=i2);
    LinFlag.use o1; LinFlag.use o2;
    unify s1 s2;
    let o12 = LinFlag.create () in
    let c12 = Mergeable.merge c1 c2 in
    (o12, s1, (i1, c12))
  in
  match out1, out2 with
  | Out(a1,b1,c1), Out(a2,b2,c2) ->
     let a3,b3,c3 = mergelocal (a1,b1,c1) (a2,b2,c2) in
     Out(a3,b3,c3)
  | OutMany(a1,b1,c1), OutMany(a2,b2,c2) ->
     let a3,b3,c3 = mergelocal (a1,b1,c1) (a2,b2,c2) in
     OutMany(a3,b3,c3)

let send : type t u. (u one * t) out -> u -> t  = fun out v ->
  match out with
  | (Out(once,channel,(k,cont))) ->
     assert (size channel = 1);
     LinFlag.use once;
     baresendone channel v;
     List.nth (Mergeable.out cont) k

let sendmany (OutMany(once,channels,(k,cont))) vf =
  LinFlag.use once;
  baresendmany channels vf;
  List.nth (Mergeable.out cont) k

let close _ = ()
