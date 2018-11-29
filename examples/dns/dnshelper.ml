open Mpst.ThreeParty
open Lwt
open Dns

let sp = Printf.sprintf
type t = {fd:Lwt_unix.file_descr;
          mutable query : Packet.t (* query *) }

let dummy_or_query =
  {label_merge=(fun l r -> object method dummy=l#dummy method query=r#query end)}

let dummy =
  {select_label=(fun f -> object method dummy=f end);
   offer_label=(fun _l -> failwith "impossible");
   channel=
     {sender=(fun (Conn _fd) () -> ()(* do nothing*));
      receiver=(fun (Conn _fd) -> Lwt.choose [])}}

let query =
  {select_label=(fun f -> object method query=f end);
   offer_label=(fun l -> `query l);
   channel=
     {sender=(fun (Conn ({fd;_} as r)) (dst, query) ->
        r.query <- query;
        let buf = Packet.marshal query in
        Lwt.async (fun () ->
            Cstruct.(Lwt_bytes.sendto fd buf.buffer buf.off buf.len [] dst)
        )
      );
      receiver=(fun (Conn {fd;_}) ->
        let buf = Cstruct.create 4096 in
        Cstruct.(Lwt_bytes.recvfrom fd buf.buffer buf.off buf.len [])
        >>= fun (len, dst) ->
        match Dns.Protocol.Server.parse (Cstruct.sub buf 0 len) with
        | None -> Lwt.choose []
        | Some query ->
           Lwt.return (dst, query)
  )}}

let answer =
  {select_label=(fun f -> object method answer=f end);
   offer_label=(fun l -> `answer l);
   channel=
     {sender=(fun (Conn {fd;_}) (dst, query, answer) ->
        let resp = Dns.Query.response_of_answer query answer in
        match Dns.Protocol.Server.marshal query resp with
        | None -> failwith "marshal failed"
        | Some buf ->
           Lwt.async (fun () ->
               Cstruct.(Lwt_bytes.sendto fd buf.buffer buf.off buf.len [] dst)
             )
      );
      receiver=(fun (Conn {fd;query;_}) ->
        let buf = Cstruct.create 4096 in
        Cstruct.(Lwt_bytes.recvfrom fd buf.buffer buf.off buf.len [])
        >>= fun (len, dst) ->
        let buf = Cstruct.sub buf 0 len in
        let p = Packet.parse buf in
        let answer = Dns.Query.answer_of_response p in
        Lwt.return (dst,query,answer))}
  }


