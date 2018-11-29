open Mpst.ThreeParty
open Lwt
open Dns

let sp = Printf.sprintf

let query =
  {select_label=(fun f -> object method query=f end);
   offer_label=(fun l -> `query l);
   channel=
     {sender=(fun (Conn fd) _query ->
        failwith "TODO"
      );
      receiver=(fun (Conn fd) ->
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
     {sender=(fun (Conn fd) (dst, query, answer) ->
        let resp = Dns.Query.response_of_answer query answer in
        match Dns.Protocol.Server.marshal query resp with
        | None -> failwith "marshal failed"
        | Some buf ->
           Lwt.async (fun () ->
               Cstruct.(Lwt_bytes.sendto fd buf.buffer buf.off buf.len [] dst)
             )
      );
      receiver=(fun (Conn _fd) ->
        Lwt.fail_with "TODO")}
  }


