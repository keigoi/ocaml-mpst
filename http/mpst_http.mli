
module Util : sig
  val http_parameter_contains : string * string -> Cohttp.Request.t -> bool
  val parse :
    Cohttp.Request.t -> (string * (string * string list) list) Lwt.t
end

type cohttp_server = {
    base_path : string;
    read_request :
      ?predicate:(Cohttp.Request.t -> bool) ->
      paths:string list ->
      unit -> (Cohttp.Request.t * Cohttp_lwt.Body.t) Lwt.t;
    write_response : Cohttp.Response.t * Cohttp_lwt.Body.t -> unit Lwt.t;
    close_server : unit -> unit Lwt.t
  }
type cohttp_client = {
    base_url : string;
    write_request :
      path:string -> params:(string * string list) list -> unit Lwt.t;
    read_response : (Cohttp.Response.t * string) Lwt.t;
    close_client : unit -> unit Lwt.t
  }
type cohttp_server_hook =
  Cohttp.Request.t ->
  Cohttp_lwt.Body.t ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) option Lwt.t

val start_server :
  string ->
  int ->
  (Cohttp_lwt_unix.Server.conn ->
   Cohttp.Request.t ->
   Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
  unit -> unit Lwt.t

val http_acceptor :  base_path:string -> (unit -> cohttp_server Lwt.t) * cohttp_server_hook
val http_connector : base_url:string -> cohttp_client Lwt.t
