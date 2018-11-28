open Mpst.ThreeParty
open Lwt
open Dns

let sp = Printf.sprintf

type t = {fd:Lwt_unix.file_descr;
          mutable dst : Unix.sockaddr; (* client address *)
          mutable query : Packet.t (* query *) }
        
let query =
  {select_label=(fun f -> object method query=f end);
   offer_label=(fun l -> `query l);
   channel=
     {sender=(fun (Conn {fd=_fd;_}) _query ->
        failwith "TODO"
      );
      receiver=(fun (Conn ({fd;_} as r)) ->
        let buf = Cstruct.create 4096 in
        Cstruct.(Lwt_bytes.recvfrom fd buf.buffer buf.off buf.len [])
        >>= fun (len, dst) ->
        match Dns.Protocol.Server.parse (Cstruct.sub buf 0 len) with
        | None -> Lwt.choose []
        | Some query ->
           r.dst <- dst;
           r.query <- query;
           Lwt.return query
  )}}

let answer =
  {select_label=(fun f -> object method answer=f end);
   offer_label=(fun l -> `answer l);
   channel=
     {sender=(fun (Conn {fd;query;dst}) answer ->
        let resp = Dns.Query.response_of_answer query answer in
        match Dns.Protocol.Server.marshal query resp with
        | None -> failwith "marshal failed"
        | Some buf ->
           Lwt.async (fun () ->
               Cstruct.(Lwt_bytes.sendto fd buf.buffer buf.off buf.len [] dst)
             )
      );
      receiver=(fun (Conn {fd=_fd;_}) ->
        Lwt.fail_with "TODO")}
  }


