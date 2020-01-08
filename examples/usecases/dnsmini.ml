open Mpst
open Usecase_util

let srv     = {role_index=Zero; role_label={make_obj=(fun v -> object method role_S=v end); call_obj=(fun o->o#role_S)}}
let cli    = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_C=v end); call_obj=(fun o->o#role_C)}}
let fwd    = {role_index=Succ (Succ Zero); role_label={make_obj=(fun v -> object method role_F=v end); call_obj=(fun o->o#role_F)}}
let to_srv m = to_ m srv srv srv
let to_cli m = to_ m cli cli cli
let to_fwd m = to_ m fwd fwd fwd

let dns () =
  (cli --> srv) query @@ (* DNS query from a client *)
  choice_at srv (to_fwd query_or_dummy) (* do I have an entry for the query?  *)
    (srv, (srv --> fwd) query @@  (* if not, forward the query to aoother server *)
          (fwd --> srv) answer @@ (* and receive a reply *)
          (srv --> cli) answer @@ (* then send back it to the client *)
          finish)
    (srv, (srv --> fwd) dummy @@ (* DUMMY (no effect)  *)
          (srv --> cli) answer @@ (* send back to the client the answer *)
          finish)
