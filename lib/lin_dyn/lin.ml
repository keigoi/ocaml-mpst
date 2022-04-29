exception InvalidEndpoint = MutexFlag.InvalidEndpoint

module Flag = MutexFlag

type flag_store = link ref
and link = Link of flag_store | FlagRef of flag ref | Unlimited
and flag = Flag.t

let rec use_flag (store : flag_store) =
  match !store with
  | Link store -> use_flag store
  | FlagRef flag -> Flag.use !flag
  | Unlimited -> ()

let rec refresh_flag (store : flag_store) =
  match !store with
  | Link store -> refresh_flag store
  | FlagRef flag -> flag := Flag.create ()
  | Unlimited -> ()

let store_unify : flag_store -> flag_store -> unit =
 fun l r ->
  if l == r then ()
  else
    match !l with
    | Unlimited ->
        assert (!r = Unlimited);
        ()
    | _ ->
        (* unify two queue endpoints, discarding the right *)
        r := Link l

type 'a lin = { value : 'a; flag : flag_store }
type 'a gen = 'a lin

let use t =
  use_flag t.flag;
  t.value

let raw_lin t = t.value
let raw_gen = raw_lin

let declare v =
  let store = FlagRef (ref @@ Flag.create ()) in
  let flag = ref store in
  { value = { value = v; flag }; flag }

let declare_unlimited v = { value = v; flag = ref Unlimited }
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
