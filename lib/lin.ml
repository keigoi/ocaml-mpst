open Concur_shims
open Base

exception InvalidEndpoint
let (let*) = IO.bind

module Flag = struct
  type t         = Mutex.t
  let create ()  = Mutex.create ()
  let use f      =
    let* b = Mutex.try_lock f in
    if b then
      IO.return ()
    else
      raise InvalidEndpoint
end

type +'a lin = {once:Flag.t; value: 'a}
type flag = Flag.t
type 's gen = flag -> 's
let use t =
  let* () = Flag.use t.once in
  IO.return t.value
let declare v = fun once -> {once; value=v}
let fresh f = f (Flag.create ())
let merge f meth x y flag =
  let x = meth.call_obj (x flag) and y = meth.call_obj (y flag) in
  assert (x.once==y.once);
  meth.make_obj {value=f x.value y.value; once=x.once}
let lift_disj mrg =
  {disj_concat=(fun ls rs once -> mrg.disj_concat (ls once) (rs once));
   disj_splitL=(fun lr once -> mrg.disj_splitL (lr once));
   disj_splitR=(fun lr once -> mrg.disj_splitR (lr once))}
let wrap f x flag = f (x flag)
