let (>>=) = Lwt.(>>=)

type 'a cohttp_server =
  {base_path: string;
   read_request:
     ?predicate:(Cohttp.Request.t -> bool)
   -> paths:string list
   -> unit
   -> (Cohttp.Request.t * Cohttp_lwt.Body.t) Lwt.t;
   write_response:
     Cohttp.Response.t * Cohttp_lwt.Body.t
     -> unit Lwt.t;
   close_server : unit -> unit Lwt.t;
   mutable extra_server : 'a
  }

type 'a cohttp_client =
  {base_url: string;
   write_request:
      path:string
   -> params:(string * string list) list
   -> unit Lwt.t;
   read_response: (Cohttp.Response.t * string) Lwt.t;
   close_client : unit -> unit Lwt.t;
   mutable extra_client : 'a
  }

let start_server host port callback () =
  let open Lwt.Infix in
  let config = Cohttp_lwt_unix.Server.make ~callback () in
  Conduit_lwt_unix.init ~src:host () >>= fun ctx ->
  let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
  Cohttp_lwt_unix.Server.create ~ctx ~mode:(`TCP (`Port port)) config

let in_mvar mvar f =
  let open Lwt.Infix in
  Lwt_mvar.take mvar >>= fun content ->
  Lwt.finalize (fun () ->
      f content) (fun () ->
      Lwt_mvar.put mvar content)

module ActionTable : sig
  type t
  val create : unit -> t
  type in_ = Cohttp.Request.t * Cohttp_lwt.Body.t
  type out = Cohttp.Response.t * Cohttp_lwt.Body.t
  val wait : t -> ?predicate:(Cohttp.Request.t -> bool) -> base_path:string -> paths:string list -> unit -> (in_ * out Lwt.u) Lwt.t
  val dispatch : t -> Cohttp.Request.t -> Cohttp_lwt.Body.t -> out option Lwt.t
end = struct
  open Lwt
  type predicate = Cohttp.Request.t -> bool
  type in_ = Cohttp.Request.t * Cohttp_lwt.Body.t
  type out = Cohttp.Response.t * Cohttp_lwt.Body.t
  type t = (string, (predicate *  (in_ * out Lwt.u) Lwt.u) list) Hashtbl.t Lwt_mvar.t

  let create () = Lwt_mvar.create (Hashtbl.create 42)
  let wait (tbl:t) ?(predicate=(fun _ -> true)) ~base_path ~paths () : (in_ * out Lwt.u) Lwt.t =
    in_mvar tbl begin fun hash ->
      let wait, wake = Lwt.wait () in
      let put path =
        let path = base_path ^ path in
        begin match Hashtbl.find_opt hash path with
        | Some xs -> Hashtbl.replace hash path ((predicate, wake)::xs)
        | None -> Hashtbl.add hash path [(predicate,wake)]
        end
      in
      List.iter put paths;
      return wait
      end >>= fun wait ->
    wait
  let dispatch (tbl:t) req body : out option Lwt.t =
    let path : string = req |> Cohttp.Request.resource |> Uri.of_string |> Uri.path in
    in_mvar tbl begin fun hash ->
      let w =
        match Hashtbl.find_opt hash path with
        | Some xs ->
           let rec loop acc = function
             | (pred,w)::xs -> if pred req
                            then (w, acc @ xs)
                            else loop ((pred,w)::acc) xs
             | [] ->
                failwith "path found but no action"
           in
           let w, xs = loop [] xs in
           Hashtbl.replace hash path xs;
           Some w
        | _ ->
           None
      in
      return w
      end >>= fun w ->
    match w with
    | Some w ->
       let wait, wake = Lwt.wait () in
       Lwt.wakeup w ((req,body), wake);
       wait >>= fun res ->
       Lwt.return (Some res)
    | None ->
       Lwt.return None
end

type cohttp_server_hook = Cohttp.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) option Lwt.t

let http_acceptor ~base_path : ('a -> 'a cohttp_server Lwt.t)  * cohttp_server_hook =
  let open Lwt in
  let table = ActionTable.create () in
  let callback req body =
    ActionTable.dispatch table req body
  in
  let acceptor ext =
    let wait, wake = Lwt.wait () in
    return
      {
         base_path;
         read_request = (fun ?predicate ~paths () ->
           ActionTable.wait table ?predicate ~base_path ~paths () >>= fun (in_, out_wake) ->
           Lwt.wakeup wake out_wake;
           return in_);
         write_response = (fun res ->
           if Lwt.state wait = Sleep
           then Lwt.fail (Failure "write: no request")
           else wait >>= fun u ->
                Lwt.return (Lwt.wakeup u res));
         close_server=(fun () -> Lwt.return ());
         extra_server=ext
       };
  in
  (acceptor, callback)

let close_server c =
  c.close_server ()

let close_client c =
  c.close_client ()

let http_connector ~(base_url : string) ext :  'a cohttp_client Lwt.t =
  let open Lwt in
  Resolver_lwt.resolve_uri ~uri:(Uri.of_string base_url) Resolver_lwt_unix.system >>= fun endp ->
  Conduit_lwt_unix.endp_to_client ~ctx:Conduit_lwt_unix.default_ctx endp >>= fun client ->
  Conduit_lwt_unix.connect ~ctx:Conduit_lwt_unix.default_ctx client >>= fun (_conn, ic, oc) ->
  let wait_input, wake_input = Lwt.wait () in
  let wait_close, wake_close = Lwt.wait () in
  return    {base_url;
             write_request = (fun ~path ~params ->
               let uri = Uri.of_string (base_url ^ path) in
               let uri = Uri.add_query_params uri params in
               Cohttp_lwt_unix.Client.call `GET uri >>= fun (resp,body) ->
               Cohttp_lwt.Body.to_string body >>= fun body ->
               Lwt.wakeup wake_input (resp, body);
               return ()
             );
             read_response = begin
                 wait_input >>= fun r ->
                 Lwt.wakeup wake_close ();
                 return r
               end;
             close_client=(fun () ->
               wait_close >>= fun () ->
               Lwt.catch (fun () ->
                   Lwt_io.close ic >>= fun () ->
                   Lwt_io.close oc
                 ) (fun _exn -> return ()));
             extra_client=ext
    }

module Util = struct
  (** http_parameter_contains ("key","value") request returns true if key=value is in the request. *)
  let http_parameter_contains (key,value) req =
    let uri = req |> Cohttp.Request.resource |> Uri.of_string in
    Uri.get_query_param uri key = Some value

  let http_parameter_key_contains key req =
    let uri = req |> Cohttp.Request.resource |> Uri.of_string in
    Uri.get_query_param uri key <> None

  (** (parse req) returns (relative_path, request_params) *)
  let parse req =
    let uri = req |> Cohttp.Request.resource |> Uri.of_string in
    Lwt.return Uri.(path uri, Uri.query uri)
end

module Labels = struct
  open Mpst_explicit
  let get =
    {obj={make_obj=(fun v -> object method get=v end);
          call_obj=(fun o -> o#get)};
     var=(fun v -> `get v)}
  let post =
    {obj={make_obj=(fun v -> object method post=v end);
          call_obj=(fun o -> o#post)};
     var=(fun v -> `post v)}
  let _302 =
    {obj={make_obj=(fun v -> object method _302=v end);
          call_obj=(fun o -> o#_302)};
     var=(fun v -> `_302 v)}
  let _200 =
    {obj={make_obj=(fun v -> object method _200=v end);
          call_obj=(fun o -> o#_200)};
     var=(fun v -> `_200 v)}
  let success =
    {obj={make_obj=(fun v -> object method success=v end);
          call_obj=(fun o -> o#success)};
     var=(fun v -> `success v)}
  let fail =
    {obj={make_obj=(fun v -> object method fail=v end);
          call_obj=(fun o -> o#fail)};
     var=(fun v -> `fail v)}
  let success_or_fail =
    {disj_merge=(fun l r -> object method success=l#success method fail=r#fail end);
     disj_splitL=(fun lr -> (lr :> <success:_>));
     disj_splitR=(fun lr -> (lr :> <fail:_>))}
end

type 'a server_pred = 'a cohttp_server -> Cohttp.Request.t -> bool
type 'a client_pred = 'a cohttp_client -> Cohttp.Response.t -> bool

let param req =
  let uri = req |> Cohttp.Request.resource |> Uri.of_string in
  Uri.query uri

let mkpred = function
  | Some f -> f
  | None -> (fun _ _ -> true)

module Handlers = struct
  open Mpst_explicit

  type http_param = (string * string list) list 

  let get ?sess_pred ?pred path =
      {write=(fun c v ->
         c.write_request ~path ~params:v);
       read=(fun c ->
         c.read_request ~paths:[path] ~predicate:(mkpred sess_pred c) () >>= fun (req,_) ->
         Lwt.return @@ param req);
       try_parse=(fun k req ->
         match pred with
         | None -> Some req
         | Some pred -> if pred k req then Some req else None
      )}

  let _200 =
    {write=(fun c v ->
       Cohttp_lwt_unix.Server.respond_string
         ~status:`OK
         ~body:v
         () >>= fun (resp,body) ->
       c.write_response (resp, body));
     read=(fun c -> (* FIXME: check resposne code *)
       c.read_response >>= fun (_resp, body) ->
       Lwt.return body);
     try_parse=(fun _ req -> Some req)}

  let _302 =
      {write=(fun c url ->
         Cohttp_lwt_unix.Server.respond_string
           ~status:`Found
           ~headers:(Cohttp.Header.init_with "Location" @@ Uri.to_string url)
           ~body:"" () >>= fun (resp,body) ->
         c.write_response (resp, body));
       read=(fun _ -> Lwt.fail_with "TODO: not implemented");
       try_parse=(fun _ v -> Some v)}
end

module SLabels = struct
  open Mpst_explicit
  let get ?sess_pred ?pred url = Labels.get %% Handlers.get ?sess_pred ?pred url
  let post ?sess_pred ?pred:_pred url = Labels.post %% Handlers.get ?sess_pred ?pred:None url (* FIXME *)
  let success ?sess_pred ?pred url = Labels.success %% Handlers.get ?sess_pred ?pred url
  let fail ?sess_pred ?pred url = Labels.fail %% Handlers.get ?sess_pred ?pred url

  let _302 = Slabel {label=Labels._302; handler=Handlers._302}
  let _200 = Slabel {label=Labels._200; handler=Handlers._200}
  let success_resp = Slabel {label=Labels.success; handler=Handlers._200}
  let fail_resp = Slabel {label=Labels.fail; handler=Handlers._200}

  let success_or_fail = Labels.success_or_fail

end
