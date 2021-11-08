open Mpst
open Usecase_util

let g () =
  fix (fun moredata ->
      (scatter mst wrk) map @@ (gather wrk mst) sum @@ moredata)
