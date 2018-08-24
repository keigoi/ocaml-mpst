open Mpst

module Util : sig
  val http_parameter_contains : string * string -> Cohttp.Request.t -> bool
  val parse :
    Cohttp.Request.t -> (string * (string * string list) list) Lwt.t
end

type 'a cohttp_server = {
    base_path : string;
    read_request :
      ?predicate:(Cohttp.Request.t -> bool) ->
      paths:string list ->
      unit -> (Cohttp.Request.t * Cohttp_lwt.Body.t) Lwt.t;
    write_response : Cohttp.Response.t * Cohttp_lwt.Body.t -> unit Lwt.t;
    close_server : unit -> unit Lwt.t;
    mutable extra_server : 'a
  }
type 'a cohttp_client = {
    base_url : string;
    write_request :
      path:string -> params:(string * string list) list -> unit Lwt.t;
    read_response : (Cohttp.Response.t * string) Lwt.t;
    close_client : unit -> unit Lwt.t;
    mutable extra_client : 'a
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

val http_acceptor :  base_path:string -> ('a -> 'a cohttp_server Lwt.t) * cohttp_server_hook
val http_connector : base_url:string -> 'a -> 'a cohttp_client Lwt.t
val close_server : 'a cohttp_server -> unit Lwt.t
val close_client : 'a cohttp_client -> unit Lwt.t

val get :
  ?pred:('a cohttp_server -> Cohttp.Request.t -> bool) ->
  string ->
  ('r1, 'a cohttp_client) Session.conn *
    ('r2, 'a cohttp_server) Session.conn ->
  (< get : (string * string list) list -> 'c >, 'c,
  [> `get of (string * string list) list * 'd ], 'd)
    Global.commm

val success :
  ?pred:('a cohttp_server -> Cohttp.Request.t -> bool) ->
  string ->
  ('r1, 'a cohttp_client) Session.conn *
    ('r2, 'a cohttp_server) Session.conn ->
  (< success : (string * string list) list -> 'c >, 'c,
      [> `success of (string * string list) list * 'd ], 'd)
    Global.commm

val fail :
  ?pred:('a cohttp_server -> Cohttp.Request.t -> bool) ->
  string ->
  ('r1, 'a cohttp_client) Session.conn *
    ('r2, 'a cohttp_server) Session.conn ->
  (< fail : (string * string list) list -> 'c >, 'c,
   [> `fail of (string * string list) list * 'd ], 'd)
    Global.commm

val post :
  'a ->
  'b * 'c ->
  (< post : 'd -> 'e >, 'e, [> `post of 'f * 'g ], 'g) Global.commm

val _200 :
  ('r1, 'a cohttp_server) Session.conn *
    ('r2, 'a cohttp_client) Session.conn ->
  (< _200 : string -> 'c >, 'c, [> `_200 of string * 'd ], 'd)
    Global.commm

val _302 :
  ('r1, 'a cohttp_server) Session.conn *
    ('r2, 'a cohttp_client) Session.conn ->
  (< _302 : Uri.t -> 'c >, 'c, [> `_302 of Uri.t * 'd ], 'd)
    Global.commm

val _200_success_fail :
  (('a, 'b) Base.either -> string) ->
  (string -> ('c, 'd) Base.either) ->
  ('r1, 'a cohttp_server) Session.conn *
    ('r2, 'a cohttp_client) Session.conn ->
  (< fail : 'b -> 'g; success : 'a -> 'h >, 'h, 'g,
                                      [> `fail of 'd * 'i | `success of 'c * 'j ], 'j, 'i)
    Global.commm2
