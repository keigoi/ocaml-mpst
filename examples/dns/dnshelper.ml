open Mpst.Global
open Mpst.Session
open Lwt

type t =
  {fd:Lwt_unix.file_descr;
   mutable peer_addr : Unix.sockaddr;
   mutable query_id : int;
  }

let create_resolver_fd ~fd ~peer_addr =
  {fd; peer_addr; query_id=0}

let create_listener_fd ~fd =
  {fd;
   peer_addr=
     Lwt_unix.(ADDR_INET (Ipaddr_unix.to_inet_addr (Ipaddr.of_string_exn "0.0.0.0"),
                         0));
   query_id=0}


let query_or_dummy =
  {label_merge=(fun l r -> object method query=l#query method dummy=r#dummy end)}

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
     {sender=(fun (Conn ({fd; peer_addr; _} as r)) query -> (* XXX move dstaddr to connection type??*)
        r.query_id <- query.Dns.Packet.id;
        let buf = Dns.Packet.marshal query in
        Lwt.async (fun () ->
            Cstruct.(Lwt_bytes.sendto fd buf.buffer buf.off buf.len [] peer_addr)
        )
      );
      receiver=(fun (Conn ({fd; _} as r)) ->
        let buf = Cstruct.create 4096 in
        Cstruct.(Lwt_bytes.recvfrom fd buf.buffer buf.off buf.len [])
        >>= fun (len, dst_addr) ->
        r.peer_addr <- dst_addr;
        match Dns.Protocol.Server.parse (Cstruct.sub buf 0 len) with
        | None -> Lwt.choose []
        | Some query ->
           Lwt.return query
  )}}

let answer =
  {select_label=(fun f -> object method answer=f end);
   offer_label=(fun l -> `answer l);
   channel=
     {sender=(fun (Conn {fd; peer_addr; _}) resp ->
        let buf = Dns.Packet.marshal resp in
        Lwt.async (fun () ->
            Cstruct.(Lwt_bytes.sendto fd buf.buffer buf.off buf.len [] peer_addr)
          )
      );
      receiver=(fun (Conn {fd; peer_addr; query_id}) ->
        let buf = Cstruct.create 4096 in
        Cstruct.(Lwt_bytes.recvfrom fd buf.buffer buf.off buf.len [])
        >>= fun (len, dst_addr) ->
        let buf = Cstruct.sub buf 0 len in
        let answer = Dns.Packet.parse buf in
        (* FIXME: check query id and dst_addr -- we need session correlation!! *)
        if peer_addr = dst_addr && answer.Dns.Packet.id = query_id then
          Lwt.return answer
        else begin
            (* FIXME: unget... and cancel the execution ... *)
            Lwt.return answer
          end
      )}
  }
