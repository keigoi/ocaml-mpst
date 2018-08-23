open Mpst.Global
open Mpst_http
open Mpst.Base
let (>>=) = Lwt.(>>=)

let http {Mpst.Session.conn=c} =
  match c with
  | Some c -> c
  | None -> failwith "mpst: http disconnected. malformed protocol?"

module HttpUtil = struct

  let get path f g (k1, k2) =
    Labels.mklabel
      f
      g
      (fun c params ->
          Lwt.async begin fun () ->
              (http c).write_request
                ~path:path
                ~params:params
            end)
      (fun c ->
        (http c).read_request ~paths:[path] () >>= fun (req, _body) ->
        Util.parse req >>= fun (path, params) ->
        Lwt.return params)
      (k1, k2)

  let get2 path pred encoder filter f g h (k1, k2) =
    Labels.mklabel2 f g h
      (fun c params ->
        Lwt.async begin fun () ->
          (http c).write_request
            ~path:path
            ~params:(encoder params)
          end)
      (fun c ->
        (http c).read_request
          ~paths:[path] ~predicate:pred () >>= fun (req, _body) ->
        Util.parse req >>= fun (path, params) ->
        filter (path, params))
      (k1, k2)
end


(* HTTP paths *)
let access_token (k1, k2) =
  HttpUtil.get
    "/access_token"
    (fun f -> object method access_token=f end)
    (fun g -> `access_token g)
    (k1, k2)

let oauth (k1, k2) =
  HttpUtil.get
    "/oauth"
    (fun g -> object method oauth=g end)
    (fun x -> `oauth x)
    (k1, k2)

let callback state (k1, k2) =
  HttpUtil.get2
    "/callback"
    (Util.http_parameter_contains ("state", state))
    (function
     | Left code -> [("code", [code]); ("state", [state])]
     | Right () -> [])
    (fun (path, query) ->
      match List.assoc_opt "code" query with
      | Some [code] -> Lwt.return (Left code)
      | _ -> Lwt.return (Right ()))
    (fun (g : string -> _) (h : unit -> _) -> object method callback=g method callback_fail=h end)
    (fun x -> `callback x) (fun x -> `callback_fail x)
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
          (http c).write_response (resp, body)
        end
    )
    (fun c ->
      (http c).read_response >>= fun (_resp, body) ->
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
          (http c).write_response (resp, body)
        end
    )
    (fun (_:(_,cohttp_client) Mpst.Session.conn) ->
      Lwt.return (failwith "TODO: not implemented" : Uri.t)) (* FIXME *)
    (k1, k2)
