exception InvalidEndpoint = MutexFlag.InvalidEndpoint

module Flag = MutexFlag
module U = Unification

type flag_store = Lin of MutexFlag.t ref U.t | Unlimited

let use_flag (store : flag_store) =
  match store with Lin u -> Flag.use @@ !(U.get u) | Unlimited -> ()

let refresh_flag (store : flag_store) =
  match store with
  | Lin store -> U.get store := Flag.create ()
  | Unlimited -> ()

let store_unify : flag_store -> flag_store -> unit =
 fun l r ->
  if l == r then ()
  else
    match (l, r) with
    | Lin l, Lin r -> U.unify l r
    | Unlimited, Unlimited -> ()
    | _ -> failwith "impossible: unlimited endpoint merged with linear endpoint"

type 'a lin = { value : 'a; flag : flag_store }
type 'a gen = 'a lin

let use t =
  use_flag t.flag;
  t.value

let raw_lin t = t.value
let raw_gen = raw_lin

let declare v =
  let flag = Lin (U.make @@ ref @@ Flag.create ()) in
  { value = { value = v; flag }; flag }

let declare_unlimited v = { value = v; flag = Unlimited }
let map_gen f x = { x with value = f x.value }
let map_lin = map_gen

let merge_lin mergefun ll rr =
  store_unify ll.flag rr.flag;
  { value = mergefun ll.value rr.value; flag = ll.flag }

let merge_gen = merge_lin

let lift_disj mrg =
  Rows.
    {
      disj_concat =
        (fun l r ->
          store_unify l.flag r.flag;
          (* track and use the same linearity flag *)
          { l with value = mrg.disj_concat l.value r.value });
      disj_splitL = (fun lr -> map_gen mrg.disj_splitL lr);
      disj_splitR = (fun lr -> map_gen mrg.disj_splitR lr);
    }

let refresh t = refresh_flag t.flag

let fresh t =
  refresh t;
  t.value
