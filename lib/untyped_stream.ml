open Types

type 'v stream = 'v Name.out * 'v Name.inp
type 'v t = {me:'v stream; othr:'v stream}

module Make(X:sig type 'a t and 'a u val fresh : 'a t -> 'a u end) = struct
  module U = Untyped.Make(X)
  
  let create () =
    {me=Name.create (fun x -> x); othr=Name.create (fun x -> x)}
  
  let flip {me=othr;othr=me} =
    {me;othr}
  
  let out_untyped ch label : 'v U.out = fun v ->
    let tag = Untyped.make_tag label.var.make_var in
    Name.send (fst ch.othr) (tag, Obj.repr v)
  
  let wrap var (v, cont) =
    var (Obj.obj v, cont)
  
  let inp_untyped ch label cont : 'var U.inp =
    let tag = Untyped.make_tag label.var.make_var in
    (fun () -> Name.receive (snd ch.me)), [(tag, U.Wrapper (cont, wrap label.var.make_var))]
  
  let wrap_list var (vs, cont) =
    var (List.map Obj.obj vs, cont)
  
  let inplist_untyped chs label cont : 'var U.inplist =
    let tag = Untyped.make_tag label.var.make_var in
    List.map (fun ch -> (fun () -> Name.receive (snd ch.me))) chs,
    [(tag, U.Wrapper (cont, wrap_list label.var.make_var))]
end[@@inline]
