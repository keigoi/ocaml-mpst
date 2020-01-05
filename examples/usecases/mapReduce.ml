open Mpst
open Usecase_util

  let mst = {role_index=Zero; role_label={make_obj=(fun v -> object method role_Mst=v end); call_obj=(fun o->o#role_Mst)}}
  let wrk = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_Wrk=v end); call_obj=(fun o->o#role_Wrk)}}

  let to_mst m = to_ m mst mst mst
  let to_wrk m = to_ m wrk wrk wrk

  let g () =
    fix (fun moredata ->
        (scatter mst wrk) map @@
          (gather wrk mst) sum @@
            moredata)
