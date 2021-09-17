open Types

type 'var inp = 'var Wrapped.wrapped_state
type 's out = unit Name.t * 's State.t

let merge_inp dst_role sl sr =
  let wl : 'a inp = dst_role.call_obj sl
  and wr : 'a inp = dst_role.call_obj sr
  in
  dst_role.make_obj (Wrapped.merge_wrapped_states wl wr)

let merge_inp_next dst_role dict s =
  let r = dst_role.call_obj s in
  Wrapped.determinise_wrapped ~dict r

let merge_out dst_role labobj sl sr =
  let (nl,sl') = labobj.call_obj @@ dst_role.call_obj sl
  and (nr,sr') = labobj.call_obj @@ dst_role.call_obj sr 
  in
  Name.unify nl nr;
  let s = State.merge sl' sr' in
  dst_role.make_obj @@ labobj.make_obj (nl, s)

let merge_out_next dst_role labobj dict s =
  let _, s = labobj.call_obj @@ dst_role.call_obj s in
  ignore @@ State.determinise ~dict s

let branch (inp:'a inp) =
  Event.sync @@ Wrapped.make_event_from_determinised_ inp
  
let select ((n,s):_ out) =
  Event.sync (Event.send (Name.finalise n) ());
  State.determinised_ s

let close () = ()
