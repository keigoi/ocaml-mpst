
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

open Explicit.Global

module Labels : sig
  type 'a server_pred = 'a cohttp_server -> Cohttp.Request.t -> bool
  type 'a client_pred = 'a cohttp_client -> Cohttp.Response.t -> bool
  val get :
    ?pred:'a server_pred ->
    < ch_get : ?pred:'a server_pred -> 'b -> ('c, 'd, 'e) channel; .. > ->
    'b ->
    (< get : 'e -> 'f >, [> `get of 'e * 'g ], 'f, 'g, 'c, 'd, 'e)
    label
  val post :
    < ch_post : 'a -> ('b, 'c, 'd) channel; .. > ->
    'a ->
    (< post : 'd -> 'e >, [> `post of 'd * 'f ], 'e, 'f, 'b, 'c, 'd)
    label
  val _302 :
    < ch_302 : ('a, 'b, 'c) channel; .. > ->
    (< _302 : 'c -> 'd >, [> `_302 of 'c * 'e ], 'd, 'e, 'a, 'b, 'c)
    label
  val _200 :
    < ch_200 : ('a, 'b, 'c) channel; .. > ->
    (< _200 : 'c -> 'd >, [> `_200 of 'c * 'e ], 'd, 'e, 'a, 'b, 'c)
    label
  val success :
    pred:'a server_pred ->
    < ch_success : 'b -> 'a server_pred -> ('c, 'd, 'e) channel; .. > ->
    'b ->
    (< success : 'e -> 'f >, [> `success of 'e * 'g ], 'f, 'g, 'c, 'd, 'e)
    label
  val fail :
    pred:'a server_pred ->
    < ch_fail : 'b -> 'a server_pred -> ('c, 'd, 'e) channel; .. > ->
    'b ->
    (< fail : 'e -> 'f >, [> `fail of 'e * 'g ], 'f, 'g, 'c, 'd, 'e)
    label
  val success_resp :
    pred:'a client_pred ->
    < ch_success_resp : 'a client_pred -> ('c, 'd, 'e) channel; .. > ->
    (< success : 'e -> 'f >, [> `success of 'e * 'g ], 'f, 'g, 'c, 'd, 'e)
    label
  val fail_resp :
    pred:'a client_pred ->
    < ch_fail_resp : 'a client_pred -> ('c, 'd, 'e) channel; .. > ->
    (< fail : 'e -> 'f >, [> `fail of 'e * 'g ], 'f, 'g, 'c, 'd, 'e)
    label
  val success_or_fail :
    (< success : 'a; .. >, < fail : 'b; .. >, < fail : 'b; success : 'a >)
    label_merge
  val fail_or_success :
    (< fail : 'a; .. >, < success : 'b; .. >, < fail : 'a; success : 'b >)
    label_merge
end

val http :
< ch_200 : ('a cohttp_server,
            'b cohttp_client, string)
           channel;
  ch_302 : ('c cohttp_server, 'd, Uri.t)
           channel;
  ch_get : ?pred:('g cohttp_server -> Cohttp.Request.t -> bool) ->
           string ->
           ('h cohttp_client,
            'g cohttp_server, (string * string list) list)
           channel;
  ch_post : string ->
            ('i cohttp_client,
             'j cohttp_server, (string * string list) list)
            channel;
  ch_success : string ->
               ('k cohttp_server -> Cohttp.Request.t -> bool) ->
               ('l cohttp_client,
                'k cohttp_server,
                (string * string list) list)
               channel;
  ch_fail : string ->
            ('e cohttp_server -> Cohttp.Request.t -> bool) ->
            ('f cohttp_client,
             'e cohttp_server, (string * string list) list)
            channel;
  ch_success_resp :
               ('l cohttp_client -> Cohttp.Response.t -> bool) ->
               ('l cohttp_server,
                'k cohttp_client,
                string)
               channel;
  ch_fail_resp : 
               ('l cohttp_client -> Cohttp.Response.t -> bool) ->
               ('l cohttp_server,
                'k cohttp_client,
                string)
                 channel
               >
