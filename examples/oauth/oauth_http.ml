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

  let get path f g (k1, k2) =
    Labels.mklabel
      f
      g
      (fun c params ->
          Lwt.async begin fun () ->
              (http c).body.write_request
                ~path:path
                ~params:params
            end)
      (fun c ->
        (http c).body.read_request ~paths:[path] () >>= fun (req, _body) ->
        Util.parse req >>= fun (path, params) ->
        Lwt.return params)
      (k1, k2)

  let get2 path pred encoder filter f g h (k1, k2) =
    Labels.mklabel2 f g h
      (fun c params ->
        Lwt.async begin fun () ->
          (http c).body.write_request
            ~path:path
            ~params:(encoder (http c) params)
          end)
      (fun c ->
        (http c).body.read_request
          ~paths:[path] ~predicate:(pred (http c)) () >>= fun (req, _body) ->
        Util.parse req >>= fun (path, params) ->
        filter (path, params))
      (k1, k2)

  let post path f g (k1, k2) = (* TODO *)
    Labels.mklabel
      f
      g
      (fun c params ->
          failwith "NOT IMPLEMENTED")
      (fun c ->
        Lwt.return (failwith "NOT IMPLEMENTED"))
      (k1, k2)
end


(* HTTP paths *)
let oauth (k1, k2) =
  HttpUtil.get
    "/oauth"
    (fun g -> object method oauth=g end)
    (fun x -> `oauth x)
    (k1, k2)

let access_token (k1, k2) =
  HttpUtil.get
    "/access_token"
    (fun f -> object method access_token=f end)
    (fun g -> `access_token g)
    (k1, k2)

let submit (k1, k2) =
  HttpUtil.post
    "/submit"
    (fun g -> object method submit=g end)
    (fun x -> `submit x)
    (k1, k2)

let success (k1, k2) =
  HttpUtil.get
    "/callback_success"
    (fun g -> object method success=g end)
    (fun x -> `success x)
    (k1, k2)

let fail (k1, k2) =
  HttpUtil.get
    "/callback_fail"
    (fun g -> object method fail=g end)
    (fun x -> `fail x)
    (k1, k2)

let success_fail (k1, k2) =
  HttpUtil.get2
    "/callback_success_fail"
    (fun c b ->
      Util.http_parameter_contains ("state", c.sessionid) b)
    (fun c ->
      function
     | Left code -> [("code", [code]); ("state", [c.sessionid])]
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
