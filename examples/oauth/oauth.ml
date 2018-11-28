(*
 * OAuth example
 *
 * a -!-> b : connect, then send.
 * b -?-> a : receive, then disconnect.
 *)
open Mpst.ThreeParty
open Mpst_http.Labels
module H = Mpst_http
let (>>=) = Lwt.(>>=)

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

type u = U
type p = P
let u = {a with role=U}
let p = {b with role=P}

let mk_oauth muc mup mcp =
  ((u,u) -!-> (c,c)) (get muc "/oauth") @@
  ((c,c) -?-> (u,u)) (_302 muc)  @@
  ((u,u) -!-> (p,p)) (get mup "-TODO-") @@
  ((p,p) -?-> (u,u)) (_200 mup) @@
  ((u,u) -!-> (p,p)) (post mup "-TODO-") @@
  choice_at p success_or_fail
    (p, ((p,p) -?-> (u,u)) (success_resp ~pred:(fun c -> failwith "TODO") mup) @@
        ((u,u) -!-> (c,c)) (success ~pred:(fun c -> H.Util.http_parameter_contains ("state", c.H.extra_server)) muc "/callback") @@
        ((c,c) -!-> (p,p)) (get mcp "/access_token") @@
        ((p,p) -?-> (c,c)) (_200 mcp) @@
        ((c,c) -?-> (u,u)) (_200 muc) @@
        finish)
    (p, ((p,p) -?-> (u,u)) (fail_resp ~pred:(fun c -> failwith "TODO") mup) @@
        ((u,u) -!-> (c,c)) (fail ~pred:(fun _ -> H.Util.http_parameter_contains ("error", "access_denied")) muc "/callback") @@
        ((c,c) -?-> (u,u)) (_200 muc) @@
        finish)

let () =
  Random.self_init ()

let facebook_oauth () =
  print_endline "oauth_consumer started";
  let g = mk_oauth H.http H.http H.http in
  let s = get_sess_ c g in
  let sessionid = Int64.to_string @@ Random.int64 Int64.max_int in
  my_acceptor sessionid >>= fun srv ->
  accept u srv s >>= fun (`get(params, s)) ->
  print_endline "connection accepted";
  let redirect_url =
    Uri.add_query_params'
      (Uri.of_string "https://www.facebook.com/dialog/oauth")
      [("client_id", Params.client_id);
       ("redirect_uri", Params.callback_url);
       ("state", sessionid)]
  in
  let s = send u (fun x->x#_302) redirect_url s in
  let s = disconnect u s in
  my_acceptor sessionid >>= fun srv ->
  accept u srv s >>= function
  | `success(_,s) ->
     H.http_connector ~base_url:"https://graph.facebook.com/v2.11/oauth" sessionid >>= fun cli ->
     let s = request p (fun x->x#get) [] cli s in
     receive p s >>= fun (`_200(_,s)) ->
     let s = disconnect p s in
     let s = send u (fun x->x#_200) "auth succeeded" s in
     let s = disconnect u s in
     close s;
     Lwt.return ()
  | `fail(_,s) ->
     let s = send u (fun x->x#_200) "auth failed" s in
     let s = disconnect u s in
     close s;
     Lwt.return ()

let () =
  Lwt_main.run @@
    let rec f () =
      Lwt.finalize (fun () ->
          facebook_oauth ())
        (fun () ->
          prerr_endline "exception";
          Lwt.return ())
      >>= fun () -> f ()
    in
    f ()
