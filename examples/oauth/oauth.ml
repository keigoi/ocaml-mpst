(*
 * OAuth
 *)
open Mpst.ThreeParty
open Mpst_http
module O = Oauth_http

(*
  $ ssh ben -R127.0.0.1:8080:127.0.0.1:8080
  $ dune build --profile=release && _build/default/ocaml-mpst/examples/oauth/oauth.exe
 *)

let (>>=) = Lwt.(>>=)

module Params = struct
  let client_id = "1491337000919429"
  let callback_url = "https://keigoimai.info/scribble/callback"
end

let acceptor, hook = http_acceptor ~base_path:"/scribble"
let () = Cohttp_server_lwt.hook := hook

let () =
  Lwt.async (Cohttp_server_lwt.start_server "/tmp/foo" 8080 "127.0.0.1" "index.html" None)

let swap (k1,k2) = k2, k1

let u = a
let p = b

let mk_oauth () =
  (u -!-> c) (O.get "/oauth") @@ (fun kuc ->
  (c --> u) (O._302 (swap kuc)) @@
  discon u c kuc @@
  (u -!-> p) (O.get "-TODO-") @@ (fun kup ->
  (p --> u) (O._200 (swap kup)) @@
  discon u p kup @@
  (u -!-> p) (O.post "-TODO-") @@ (fun kup ->
  (p -%%-> u) (O.success_fail (fun _ -> failwith "") (fun _ -> failwith "") kup)
    ~l1:((p,u), discon u p kup @@
                (u -!-> c) (O.success ~pred:(fun c -> Util.http_parameter_contains ("state", c.O.sessionid)) "/callback") (fun kuc ->
                (c -!-> p) (O.get "/access_token") (fun kcp ->
                (p --> c) (O._200 (swap kcp)) @@
                discon c p kcp @@
                (c --> u) (O._200 (swap kuc)) @@
                discon u c kuc @@
                finish)))
    ~l2:((p,u), discon u p kup @@
                (u -!-> c) (O.fail ~pred:(fun _ -> Util.http_parameter_contains ("error", "access_denied")) "/callback") (fun kuc ->
                (c --> u) (O._200 (swap kuc)) @@
                discon u c kuc @@
                finish)))))

let () =
  Random.self_init ()

let facebook_oauth () =
  print_endline "oauth_consumer started";
  let g = mk_oauth () in
  let s = get_sess c g in
  acceptor () >>= fun srv ->
  let sessionid = Int64.to_string @@ Random.int64 Int64.max_int in
  let srv = {O.sessionid; body=srv} in
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
  disconnect A (fun k -> k.O.body.close_server ()) s >>= fun s ->
  acceptor () >>= fun srv' ->
  accept A {srv with O.body=srv'} s >>= function
  | `success(_,s) ->
     http_connector ~base_url:"https://graph.facebook.com/v2.11/oauth" >>= fun cli ->
     let s = request B (fun x->x#get) [] {O.sessionid=""; body=cli} s in
     receive B s >>= fun (`_200(_,s)) ->
     disconnect B (fun k -> k.O.body.close_client ()) s >>= fun s ->
     let s = send A (fun x->x#_200) "auth succeeded" s in
     disconnect A (fun k -> k.O.body.close_server ()) s >>= fun s ->
     close s;
     Lwt.return ()
  | `fail(_,s) ->
     let s = send A (fun x->x#_200) "auth failed" s in
     disconnect A (fun k -> k.O.body.close_server ()) s >>= fun s ->
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
