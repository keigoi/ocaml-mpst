open Mpst.Global
open Mpst.Session
open Lwt

let sp = Printf.sprintf

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
     {sender=(fun (Conn fd) (dst_addr, query) -> (* XXX move dstaddr to connection type??*)
        let buf = Dns.Packet.marshal query in
        Lwt.async (fun () ->
            Cstruct.(Lwt_bytes.sendto fd buf.buffer buf.off buf.len [] dst_addr)
        )
      );
      receiver=(fun (Conn fd) ->
        let buf = Cstruct.create 4096 in
        Cstruct.(Lwt_bytes.recvfrom fd buf.buffer buf.off buf.len [])
        >>= fun (len, dst_addr) ->
        match Dns.Protocol.Server.parse (Cstruct.sub buf 0 len) with
        | None -> Lwt.choose []
        | Some query ->
           Lwt.return (dst_addr, query)
  )}}

let answer =
  {select_label=(fun f -> object method answer=f end);
   offer_label=(fun l -> `answer l);
   channel=
     {sender=(fun (Conn fd) (dst_addr, resp) ->
        let buf = Dns.Packet.marshal resp in
        Lwt.async (fun () ->
            Cstruct.(Lwt_bytes.sendto fd buf.buffer buf.off buf.len [] dst_addr)
          )
      );
      receiver=(fun (Conn fd) ->
        let buf = Cstruct.create 4096 in
        Cstruct.(Lwt_bytes.recvfrom fd buf.buffer buf.off buf.len [])
        (* FIXME: check query id -- we need session correlation!! *)
        >>= fun (len, dst_addr) ->
        let buf = Cstruct.sub buf 0 len in
        let answer = Dns.Packet.parse buf in
        Lwt.return (dst_addr, answer))}
  }
