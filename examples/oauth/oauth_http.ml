(* TODO: implement the POST method *)
open Mpst.Global
open Mpst_http
open Mpst.Base
let (>>=) = Lwt.(>>=)

type 'c oauth_session={mutable sessionid:string; body:'c}

let http {Mpst.Session.conn=c} =
  match c with
  | Some c -> c
  | None -> failwith "mpst: http disconnected. malformed protocol?"

module HttpUtil = struct

end


let get_ f g path ?(pred=(fun _ _->true)) (k1, k2) =
  Labels.mklabel
    f g
    (fun c params ->
      Lwt.async begin fun () ->
        (http c).body.write_request
          ~path:path
          ~params:params
        end)
    (fun c ->
      (http c).body.read_request ~paths:[path] ~predicate:(pred (http c)) () >>= fun (req, _body) ->
      Util.parse req >>= fun (path, params) ->
      Lwt.return params)
    (k1, k2)

let get ?pred path k12 =
  get_
    (fun f -> object method get=f end)
    (fun x -> `get x)
    path ?pred k12

let success ?pred path k12 =
  get_
    (fun f -> object method success=f end)
    (fun x -> `success x)
    path ?pred k12

let fail ?pred path k12 =
  get_
    (fun f -> object method fail=f end)
    (fun x -> `fail x)
    path ?pred k12

let post path (k1, k2) = (* TODO *)
  Labels.mklabel
    (fun g -> object method post=g end)
    (fun x -> `post x)
    (fun c params ->
      failwith "NOT IMPLEMENTED")
    (fun c ->
      Lwt.return (failwith "NOT IMPLEMENTED"))
    (k1, k2)

(* HTTP response *)
let _200 (k1, k2) =
  Labels.mklabel
    (fun f -> object method _200=f end)
    (fun x -> `_200 x)
    (fun c page ->
      Lwt.async begin fun () ->
          Cohttp_lwt_unix.Server.respond_string
            ~status:`OK
            ~body:page
            () >>= fun (resp,body) ->
          (http c).body.write_response (resp, body)
        end
    )
    (fun c ->
      (http c).body.read_response >>= fun (_resp, body) ->
      Lwt.return body)
    (k1, k2)

let _302 (k1, k2) =
  Labels.mklabel
    (fun f -> object method _302=f end)
    (fun x -> `_302 x)
    (fun c url ->
      Lwt.async begin fun () ->
          Cohttp_lwt_unix.Server.respond_string
            ~status:`Found
            ~headers:(Cohttp.Header.init_with "Location" @@ Uri.to_string url)
            ~body:"" () >>= fun (resp,body) ->
          (http c).body.write_response (resp, body)
        end
    )
    (fun (_:(_,cohttp_client oauth_session) Mpst.Session.conn) ->
      Lwt.return (failwith "TODO: not implemented" : Uri.t)) (* FIXME *)
    (k1, k2)

let success_fail make_page parse_page (k1, k2) =
  Labels.mklabel2
    (fun f g -> object method success=f method fail=g end)
    (fun x -> `success x)
    (fun x -> `fail x)
    (fun c v ->
      Lwt.async begin fun () ->
          Cohttp_lwt_unix.Server.respond_string
            ~status:`OK
            ~body:(make_page v)
            () >>= fun (resp,body) ->
          (http c).body.write_response (resp, body)
        end)
    (fun c ->
      (http c).body.read_response >>= fun (_resp, body) ->
      Lwt.return @@ parse_page body) (* FIXME *)
    (k1, k2)
