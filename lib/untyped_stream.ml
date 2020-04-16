open Base

  type 'v stream = 'v Stream_opt.t
  type 'v t = {me:'v stream; othr:'v stream}
  
  let create () =
    {me=Stream_opt.create (); othr=Stream_opt.create ()}
  
  let flip {me=othr;othr=me} =
    {me;othr}

module Make(X:sig type 'a t and 'a u val fresh : 'a t -> 'a u end) = struct
  module U = Untyped.Make(X)
  
  let out_untyped ch label : 'v U.out = fun v ->
    let tag = Untyped.make_tag label.var in
    Stream_opt.send ch.othr (tag, Obj.repr v)
  
  let wrap var (v, cont) =
    var (Obj.obj v, cont)
  
  let inp_untyped ch label cont : 'var U.inp =
    let tag = Untyped.make_tag label.var in
    (fun () -> Stream_opt.receive ch.me), [(tag, U.Wrapper (cont, wrap label.var))]
  
  let wrap_list var (vs, cont) =
    var (List.map Obj.obj vs, cont)
  
  let inplist_untyped chs label cont : 'var U.inplist =
    let tag = Untyped.make_tag label.var in
    List.map (fun ch -> (fun () -> Stream_opt.receive ch.me)) chs,
    [(tag, U.Wrapper (cont, wrap_list label.var))]
end
