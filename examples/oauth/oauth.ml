(*
 * OAuth example
 *
 * a -!-> b : connect, then send.
 * b -?-> a : receive, then disconnect.
 *)
open Mpst_explicit
module H = Mpst_http

open Mpst_http.SLabels

let (let/) = Lwt.(>>=)

(*
  (replace keigoimai.info with appropriate domain)

  $ opam install lwt_ssl
  $ ssh ben -R127.0.0.1:8080:127.0.0.1:8080
  $ dune build --profile=release examples/oauth/oauth.exe
  $ _build/default/examples/oauth/oauth.exe

  then open https://keigoimai.info/scribble/oauth
 *)
(*
  In keigoimai.info, ssl.conf:

    ProxyPass /scribble http://127.0.0.1:8080/scribble
    ProxyPassReverse /scribble http://127.0.0.1:8080/scribble
 *)

module Params = struct
  let client_id = "1491337000919429"
  let callback_url = "https://keigoimai.info/scribble/callback"
end

(* prepare a HTTP server *)
let my_acceptor, hook = H.http_acceptor ~base_path:"/scribble"
let () =
  Cohttp_server_lwt.hook := hook;
  Lwt.async (Cohttp_server_lwt.start_server "/var/empty" 8080 "127.0.0.1" "index.html" None)

let u = Role {role_label=
                {make_obj=(fun v->object method role_U=v end);
                 call_obj=(fun o->o#role_U)};
              role_index=Zero; role_index0=Zero}
let c = Role {role_label=
                {make_obj=(fun v->object method role_C=v end);
                 call_obj=(fun o->o#role_C)};
              role_index=Succ Zero; role_index0=Succ Zero}
let p = Role {role_label=
                {make_obj=(fun v->object method role_P=v end);
                 call_obj=(fun o->o#role_P)};
              role_index=Succ (Succ Zero); role_index0=Succ (Succ Zero)}
let to_u m = to_ m u u u
let to_c m = to_ m c c c
let to_p m = to_ m p p p

let mk_oauth () =
  let sess_pred c =
    H.Util.http_parameter_contains ("state", c.H.extra_server)
  in
  let success_pred c params =
    not (List.mem_assoc "error" params)
  in
  let error_pred c params =
    List.mem_assoc "error" params
  in
  (u -!-> c) (get "/oauth") @@
  (c -?-> u) _302  @@
  (u -!-> p) (get "-TODO-") @@
  (p -?-> u) _200 @@
  (u -!-> p) (post "-TODO-") @@
  choice_at p (to_u success_or_fail)
    (p, (p -?-> u) success_resp @@
        (u -!-> c) (success ~sess_pred ~pred:success_pred "/callback") @@
        (c -!-> p) (get "/access_token") @@
        (p -?-> c) _200 @@
        (c -?-> u) _200 @@
        finish)
    (p, (p -?-> u) fail_resp @@
        (u -!-> c) (fail ~sess_pred ~pred:error_pred "/callback") @@
        (c -?-> u) _200 @@
        finish)

let () =
  Random.self_init ()

let facebook_oauth () =
  print_endline "oauth_consumer started";
  let g = mk_oauth () in
  let s = get_ep c g HList.vec_all_empty in
  let sessionid = Int64.to_string @@ Random.int64 Int64.max_int in
  let/ srv = my_acceptor sessionid in
  let/ `get(params, s) = receive (s srv)#role_U in
  print_endline "connection accepted";
  let redirect_url =
    Uri.add_query_params'
      (Uri.of_string "https://www.facebook.com/dialog/oauth")
      [("client_id", Params.client_id);
       ("redirect_uri", Params.callback_url);
       ("state", sessionid)]
  in
  let/ s = s#role_U#_302 redirect_url in
  let/ srv = my_acceptor sessionid in
  let/ res = receive (s srv)#role_U in
  match res with
  | `success(_,s) ->
     let/ cli = H.http_connector ~base_url:"https://graph.facebook.com/v2.11/oauth" sessionid in
     let/ s = (s cli)#role_P#get [] in
     let/ `_200(_,s) = receive (s#role_P) in
     let/ s = s#role_U#_200 "auth succeeded" in
     close s
  | `fail(_,s) ->
     let/ s = s#role_U#_200 "auth failed" in
     close s

let () =
  Lwt_main.run @@
    let rec loop () =
      let/ () =
        Lwt.finalize
          (fun () -> facebook_oauth ())
          (fun () -> prerr_endline "exception"; Lwt.return ())
      in
      loop ()
    in
    loop ()
