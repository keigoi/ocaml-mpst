open Mpst.ThreeParty
open Mpst_http
module O = Oauth_http

let (>>=) = Lwt.(>>=)

module Params = struct
  let base_url = "https://graph.facebook.com/v2.11/oauth"
  let oauth_start_url = "https://www.facebook.com/dialog/oauth"
  let client_id = "1491337000919429"
  let callback_url = "https://keigoimai.info/scribble/callback"
  let client_secret = "*****"
end

let acceptor, hook = http_acceptor ~base_path:"/scribble"


let () =
  Lwt.async (Cohttp_server_lwt.start_server "/tmp/foo" 8080 "lab.keigoimai.info" "index.html" None)


let () = Cohttp_server_lwt.hook := hook

let swap (k1,k2) = k2, k1

let u = a
let p = b

let mk_oauth () =
  (u -!-> c) O.oauth @@ (fun kuc ->
  (c --> u) (O._302 (swap kuc)) @@
  discon u c kuc @@
  (u -!-> p) O.oauth @@ (fun kup ->
  (p --> u) (O._200 (swap kup)) @@
  discon u p kup @@
  (u -!-> p) O.submit @@ (fun kup ->
  (p -!%%-> u) O.success_fail
    ~l1:((p,u), fun kuc ->
                discon u p kup @@
                (u -!-> c) O.success (fun kuc ->
                (c -!-> p) O.access_token (fun kcp ->
                (p --> c) (O._200 (swap kcp)) @@
                discon c p kcp @@
                (c --> u) (O._200 (swap kuc)) @@
                discon u c kuc @@
                finish)))
    ~l2:((p,u), fun kuc ->
                discon u p kup @@
                (u -!-> c) O.fail (fun kuc ->
                (c --> u) (O._200 (swap kuc)) @@
                discon u c kuc @@
                finish)))))

let consumer () =
  let g = mk_oauth () in
  let s = get_sess c g in
  acceptor () >>= fun srv ->
  let srv = {O.sessionid=""; body=srv} in
  accept A srv s >>= fun (`oauth(params, s)) ->
  let s = send A (fun x->x#_302) (failwith "uri") s in
  disconnect A (fun k -> k.O.body.close_server ()) s >>= fun s ->
  acceptor () >>= fun srv' ->
  accept A {srv with O.body=srv'} s >>= function
  | `success(_,s) ->
     http_connector ~base_url:"https://graph.facebook.com/v2.11/oauth" >>= fun cli ->
     let s = request B (fun x->x#access_token) [] {O.sessionid=""; body=cli} s in
     receive B s >>= fun (`_200(_,s)) ->
     disconnect B (fun k -> k.O.body.close_client ()) s >>= fun s ->
     let s = send A (fun x->x#_200) "auth succeeded" s in
     disconnect A (fun k -> k.O.body.close_server ()) s >>= fun s ->
     close s;
     Lwt.return s
  | `fail(_,s) ->
     let s = send A (fun x->x#_200) "auth failed" s in
     disconnect A (fun k -> k.O.body.close_server ()) s >>= fun s ->
     close s;
     Lwt.return s
