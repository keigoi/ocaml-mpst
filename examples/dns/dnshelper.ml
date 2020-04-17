open Mpst_plug
open Lwt

let cli = Role {role_label=
                {make_obj=(fun v->object method role_Cli=v end);
                 call_obj=(fun o->o#role_Cli)};
              role_index=Zero; role_index0=Zero}
let srv = Role {role_label=
                {make_obj=(fun v->object method role_Srv=v end);
                 call_obj=(fun o->o#role_Srv)};
              role_index=Succ Zero; role_index0=Succ Zero}
let fwd = Role {role_label=
                {make_obj=(fun v->object method role_Fwd=v end);
                 call_obj=(fun o->o#role_Fwd)};
              role_index=Succ (Succ Zero); role_index0=Succ (Succ Zero)}
let to_cli m = to_ m cli cli cli
let to_srv m = to_ m srv srv srv
let to_fwd m = to_ m fwd fwd fwd

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
  {disj_merge=(fun l r -> object method query=l#query method dummy=r#dummy end);
   disj_splitL=(fun lr -> (lr :> <query:_>));
   disj_splitR=(fun lr -> (lr :> <dummy:_>));
  }

let dummy =
  Slabel
    {label={obj={make_obj=(fun f -> object method dummy=f end);
                 call_obj=(fun f -> f#dummy)};
            var=(fun v -> `dummy(v))};
     handler=
       {write=(fun _fd () -> Lwt.return ()(* do nothing*));
        read=(fun _fd -> Lwt.return None);
        try_parse=(fun _ _ -> None)
       }
    }

let query =
  Slabel
    {label={obj={make_obj=(fun f -> object method query=f end);
                 call_obj=(fun f -> f#query)};
            var=(fun v -> `query(v))};
     handler=
       {write=(fun ({fd; peer_addr; _} as r) query -> (* XXX move dstaddr to connection type??*)
          r.query_id <- query.Dns.Packet.id;
          let buf = Dns.Packet.marshal query in
          Cstruct.(Lwt_bytes.sendto fd buf.buffer buf.off buf.len [] peer_addr) >>= fun _ ->
          Lwt.return_unit
      );
      read=(fun ({fd; _} as r) ->
        let buf = Cstruct.create 4096 in
        Cstruct.(Lwt_bytes.recvfrom fd buf.buffer buf.off buf.len [])
        >>= fun (len, dst_addr) ->
        r.peer_addr <- dst_addr;
        (*fixme bufferiing*)
        Lwt.return (Dns.Protocol.Server.parse (Cstruct.sub buf 0 len))
        );
      try_parse=(fun _ o -> o)
    }}

let answer =
  Slabel
    {label={obj={make_obj=(fun f -> object method answer=f end);
                 call_obj=(fun f -> f#answer)};
            var=(fun v -> `answer(v))};
     handler=
       {write=(fun {fd; peer_addr; _} resp ->
          let buf = Dns.Packet.marshal resp in
          Cstruct.(Lwt_bytes.sendto fd buf.buffer buf.off buf.len [] peer_addr) >>= fun _ ->
          Lwt.return_unit
        );
        read=(fun {fd; peer_addr; query_id} ->
          let buf = Cstruct.create 4096 in
          Cstruct.(Lwt_bytes.recvfrom fd buf.buffer buf.off buf.len [])
          >>= fun (len, dst_addr) ->
          let buf = Cstruct.sub buf 0 len in
          let answer = Dns.Packet.parse buf in
          (* FIXME: check query id and dst_addr -- we need session correlation!! *)
          if peer_addr = dst_addr && answer.Dns.Packet.id = query_id then
            Lwt.return (Some answer)
          else begin
              (* FIXME: unget... and cancel the execution ... *)
              Lwt.return (Some answer)
            end);
        try_parse=(fun _ o -> o)}}
