open Concur_shims
open Base

let (let*) = IO.bind

type pipe = {inp: IO.in_channel; out: IO.out_channel}
type t = {me:pipe; othr:pipe}

let[@inline] ignore_exn f x =
  IO.catch (fun[@inline] () -> f x) (fun[@inline] _ -> IO.return_unit)

let[@inline] close_dpipe dp =
  let* () = ignore_exn IO.close_in dp.me.inp in
  let* () = ignore_exn IO.close_out dp.me.out in
  IO.return_unit

module Make(X:sig type 'a t and 'a u val fresh : 'a t -> 'a u end) = struct
  module U = Untyped.Make(X)

  let new_dpipe () =
    let my_inp, otr_out = IO.pipe () in
    let otr_inp, my_out = IO.pipe () in
    {me={inp=my_inp; out=my_out};
     othr={inp=otr_inp; out=otr_out}}

  let flip {me=othr;othr=me} =
    {me;othr}

  let close_dpipe = close_dpipe


  let[@Inline] out_dpipe ch label : 'v U.out = fun v ->
    let tag = Untyped.make_tag label.var in
    let* () = IO.output_value ch.me.out (tag, Obj.repr v) in
    IO.flush ch.me.out

  let[@inline] wrap var (v, cont) =
    var (Obj.obj v, cont)

  let[@inline] inp_dpipe ch label cont : 'var U.inp =
    let tag = Untyped.make_tag label.var in
    (fun[@inline] () -> let* x = IO.input_value ch.me.inp in IO.return x),
    [(tag, U.Wrapper (cont, wrap label.var))]

  let[@inline] wrap_list var (vs, cont) =
    var (List.map Obj.obj vs, cont)

  let[@inline] inplist_dpipe chs label cont : 'var U.inplist =
    let tag = Untyped.make_tag label.var in
    List.map (fun[@inline] ch -> (fun[@inline] () -> IO.input_value ch.me.inp)) chs,
    [(tag, U.Wrapper (cont, wrap_list label.var))]
end
