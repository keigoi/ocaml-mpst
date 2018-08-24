(*
 * OAuth example
 *
 * a -!-> b : connect, then send.
 * b -?-> a : receive, then disconnect.
 *)
open Mpst.ThreeParty
module H = Mpst_http
let (>>=) = Lwt.(>>=)

(*
  $ ssh ben -R127.0.0.1:8080:127.0.0.1:8080
  $ dune build --profile=release && _build/default/ocaml-mpst/examples/oauth/oauth.exe
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

let u = a
let p = b

let mk_oauth () =
  (u -!-> c) (H.get "/oauth") @@ (fun kuc ->
  (c -?-> u) (H._302 (swap kuc)) kuc @@
  (u -!-> p) (H.get "-TODO-") @@ (fun kup ->
  (p -?-> u) (H._200 (swap kup)) kup @@
  (u -!-> p) (H.post "-TODO-") @@ (fun kup ->
  (p -?%%-> u) (H._200_success_fail (fun _ -> (*TODO*)failwith "") (fun _ -> (*TODO*)failwith "") kup) kup
    ~l1:((p,u), (u -!-> c) (H.success ~pred:(fun c -> H.Util.http_parameter_contains ("state", c.H.extra_server)) "/callback") (fun kuc ->
                (c -!-> p) (H.get "/access_token") (fun kcp ->
                (p -?-> c) (H._200 (swap kcp)) kcp @@
                (c -?-> u) (H._200 (swap kuc)) kuc @@
                finish)))
    ~l2:((p,u), (u -!-> c) (H.fail ~pred:(fun _ -> H.Util.http_parameter_contains ("error", "access_denied")) "/callback") (fun kuc ->
                (c -?-> u) (H._200 (swap kuc)) kuc @@
                finish)))))

let () =
  Random.self_init ()

let facebook_oauth () =
  print_endline "oauth_consumer started";
  let g = mk_oauth () in
  let s = get_sess c g in
  let sessionid = Int64.to_string @@ Random.int64 Int64.max_int in
  my_acceptor sessionid >>= fun srv ->
  accept A srv s >>= fun (`get(params, s)) ->
  print_endline "connection accepted";
  let redirect_url =
    Uri.add_query_params'
      (Uri.of_string "https://www.facebook.com/dialog/oauth")
      [("client_id", Params.client_id);
       ("redirect_uri", Params.callback_url);
       ("state", sessionid)]
  in
  let s = send A (fun x->x#_302) redirect_url s in
  disconnect A H.close_server s >>= fun s ->
  my_acceptor sessionid >>= fun srv ->
  accept A srv s >>= function
  | `success(_,s) ->
     H.http_connector ~base_url:"https://graph.facebook.com/v2.11/oauth" sessionid >>= fun cli ->
     let s = request B (fun x->x#get) [] cli s in
     receive B s >>= fun (`_200(_,s)) ->
     disconnect B (fun k -> k.H.close_client ()) s >>= fun s ->
     let s = send A (fun x->x#_200) "auth succeeded" s in
     disconnect A (fun k -> k.H.close_server ()) s >>= fun s ->
     close s;
     Lwt.return ()
  | `fail(_,s) ->
     let s = send A (fun x->x#_200) "auth failed" s in
     disconnect A (fun k -> k.H.close_server ()) s >>= fun s ->
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
