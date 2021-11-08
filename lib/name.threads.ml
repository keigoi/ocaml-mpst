open Concur_shims

type 'a ch = 'a ch_ ref

and 'a ch_ = { channel : 'a Event.channel; merged : 'a ch list }

type 'a out = 'a ch
type 'a inp = 'a Event.event
type 'a out_many = 'a out list

type ('w, 'u) gather0 = {
  g_inplist : 'u inp list;
  g_wrap : 'u list -> 'w;
  g_merged : 'w inp_many list;
}

and _ gather_ = Gather : ('w, 'u) gather0 -> 'w gather_

and 'w inp_many = 'w gather_ ref

type ('a, 'b) either = Left of 'a | Right of 'b

let create f =
  let evch = Event.new_channel () in
  let rec ch = { contents = { channel = evch; merged = [ ch ] } } in
  let inp = ch in
  (ch, Event.wrap (Event.guard (fun () -> Event.receive !inp.channel)) f)

let merge_out cl cr =
  if cl == cr then cl
  else
    let cl0, cr0 = (!cl, !cr) in
    let newch = { channel = cl0.channel; merged = cl0.merged @ cr0.merged } in
    cl := newch;
    cr := newch;
    cl

let merge_inp cl cr = if cl == cr then cl else Event.choose [ cl; cr ]
let send c v = Event.sync (Event.send !c.channel v)
let receive c = Event.sync c

let create_out_many cnt f =
  let outs, inps = List.split @@ List.init cnt (fun i -> create (f i)) in
  (outs, inps)

let merge_out_many outLs outRs = List.map2 merge_out outLs outRs
let send_many outs f = List.iteri (fun i out -> send out (f i)) outs

let create_inp_many cnt (f : 'v list -> 't) =
  let outs, inps = List.split @@ List.init cnt (fun _ -> create (fun x -> x)) in
  let rec gather =
    {
      contents = Gather { g_inplist = inps; g_wrap = f; g_merged = [ gather ] };
    }
  in
  (outs, gather)

let hetero_merge_inp : type t u. t inp -> u inp -> (t, u) either inp =
 fun cl cr ->
  Event.choose
    [ Event.wrap cl (fun x -> Left x); Event.wrap cr (fun y -> Right y) ]

let merge_inp_many : type t. t inp_many -> t inp_many -> t inp_many =
 fun gl gr ->
  match (gl, gr) with
  | ( {
        contents =
          Gather { g_inplist = inpsL; g_wrap = wrapL; g_merged = mergedL };
      },
      {
        contents =
          Gather { g_inplist = inpsR; g_wrap = wrapR; g_merged = mergedR };
      } ) ->
      let inps = List.map2 hetero_merge_inp inpsL inpsR in
      let wrap_left = function
        | Left x -> x
        | Right _ -> failwith "gather: reception failure: Right"
      and wrap_right = function
        | Right x -> x
        | Left _ -> failwith "gather: reception failure: Left"
      in
      let wrap = function
        | Left x :: xs -> wrapL @@ (x :: List.map wrap_left xs)
        | Right x :: xs -> wrapR @@ (x :: List.map wrap_right xs)
        | [] -> failwith "gather: reception failure: empty"
      in
      let merged = mergedL @ mergedR in
      let g0 = Gather { g_inplist = inps; g_wrap = wrap; g_merged = merged } in
      List.iter (fun g -> g := g0) merged;
      gl

let receive_many : type t. t inp_many -> t IO.io = function
  | { contents = Gather { g_inplist = inps; g_wrap = wrap; _ } } ->
      wrap (List.map receive inps)
