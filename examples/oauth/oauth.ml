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

let () = Cohttp_server_lwt.hook := hook

let swap (k1,k2) = k2, k1

let mk_oauth () =
  (a -!-> b) O.oauth @@ (fun kab ->
  (b --> a) (O._302 (swap kab)) @@
  discon a b kab @@
  (* (a -!-> b) O.callback @@ (fun kab -> .. we need explicit connection + branching *)

  finish)
