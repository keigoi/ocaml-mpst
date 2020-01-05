open Mpst
open Usecase_util

  let mst = {role_index=Zero; role_label={make_obj=(fun v -> object method role_Mst=v end); call_obj=(fun o->o#role_Mst)}}
  let wrk = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_Wrk=v end); call_obj=(fun o->o#role_Wrk)}}

  let to_mst m = to_ m mst mst mst
  let to_wrk m = to_ m wrk wrk wrk

  let reply () =
    fix (fun t ->
        choice_at wrk (to_mst result_or_done)
          (wrk, (wrk --> mst) result @@ t)
          (wrk, (wrk --> mst) done_ @@ finish))

  let g () =
    fix (fun t ->
        choice_at mst (to_wrk work_or_stop)
          (mst, (scatter mst wrk) (work >>: prot wrk (reply ())) @@
                  t)
          (mst, (scatter mst wrk) stop @@
                  (gather wrk mst) stop @@ finish))
