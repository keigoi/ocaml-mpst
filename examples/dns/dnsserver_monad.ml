module S = Mpst.Linsession
module Monad = Mpst.Monad

open Dnshelper

(* FIXME!!! *)
let myip = "172.21.24.215"
let myport = 12345

let forward_ip = "8.8.8.8"
let forward_port = 53

(*
  $ opam install dns-lwt-unix
  $ dune build --profile=release examples/dns/dnsserver.exe
  $ sudo _build/default/examples/dns/dnsserver.exe

  (in another window)
  $ host nagoya.my.domain 127.0.0.1
  $ host www.google.com 127.0.0.1
 *)

module Roles = struct
  type cli = Cli
  type srv = Srv
  type oth = Oth
end

let cli = {Mpst.ThreeParty.a with role=Roles.Cli}
let srv = {Mpst.ThreeParty.b with role=Roles.Srv}
let fwd = {Mpst.ThreeParty.c with role=Roles.Oth}

let dns () =
  let open Mpst.ThreeParty in
  ((cli,cli) -!-> (srv,srv)) query @@ (* DNS query from a client *)
  choice_req_at srv query_or_dummy (* do I have an entry for the query?  *)
    (srv, ((srv,srv) -!-> (fwd,fwd)) query @@  (* if not, forward the query to aoother server *)
          ((fwd,fwd) -?-> (srv,srv)) answer @@ (* and receive a reply *)
          ((srv,srv) -?-> (cli,cli)) answer @@ (* then send back it to the client *)
          finish_)
    (srv, ((srv,srv) -!-> (fwd,fwd)) dummy @@ (* DUMMY (no effect)  *)
          discon (srv,srv) (fwd,fwd) @@       (* DUMMY (no effect) *)
          ((srv,srv) -?-> (cli,cli)) answer @@ (* send back to the client the answer *)
          finish_)

let addresses = [
  Dns.Name.of_string "nagoya.my.domain", Ipaddr.V4.of_string_exn "1.2.3.4";
  Dns.Name.of_string "gifu.my.domain", Ipaddr.V4.of_string_exn "1.2.3.5";
]

let nxdomain =
  Dns.Query.({ rcode = Dns.Packet.NXDomain; aa = true; answer = []; authority = []; additional = [] })

let lookup ({ Dns.Packet.q_name; _ } as question) =
  let (>>=) = Lwt.(>>=) in
  if List.mem_assoc q_name addresses then begin
      let ip = List.assoc q_name addresses in
      Lwt_io.printf "DNS: %s is a builtin: %s\n"
        (Dns.Packet.question_to_string question)
        Ipaddr.V4.(to_string ip)
      >>= fun () ->
      let rrs = [ { Dns.Packet.name = q_name; cls = RR_IN; flush = false; ttl = 0l; rdata = A ip } ] in
      Lwt.return (Some (Dns.Query.({ rcode = NoError; aa = true; answer = rrs; authority = []; additional = [] })))
    end else begin
      Lwt.return None
    end

let _put v = Monad.put Mpst.Base.root v

let server (fd_cli : Lwt_unix.file_descr) (fd_fwd : Lwt_unix.file_descr) =
  let (>>=) = Monad.(>>=) in
  let fwd_addr =
    Lwt_unix.(ADDR_INET (Ipaddr_unix.to_inet_addr (Ipaddr.of_string_exn forward_ip),
                         forward_port))
  in
  let fd_fwd = create_resolver_fd ~fd:fd_fwd ~peer_addr:fwd_addr in
  let fd_cli = create_listener_fd ~fd:fd_cli in
  let s = Mpst.ThreeParty.get_sess_ srv (dns ()) in
  let rec loop () =
    Lwt.bind (
    Monad.__run s begin
      S.accept cli fd_cli >>= fun (`query(query, tmp)) -> _put tmp >>= fun () ->
      begin match query.Dns.Packet.questions with
      | ({Dns.Packet.q_class = Q_IN; q_type = Q_A; _} as question) :: _ ->
         Monad.lift (lookup question) >>= fun answer ->
         begin match answer with
         | Some answer ->
            S.request fwd (fun x->x#dummy) () fd_fwd >>= fun tmp -> _put tmp >>= fun () -> (* no effect *)
            S.disconnect fwd >>= fun tmp -> _put tmp >>= fun () -> (* no effect *)
            Monad.return answer
         | None ->
            let fwd_query =
              Dns.Query.create
                ~id:(Random.int (1 lsl 16))
                ~dnssec:false
                question.q_class question.q_type question.q_name
            in
            S.request fwd (fun x->x#query) fwd_query fd_fwd >>= fun tmp -> _put tmp >>= fun () ->
            S.receive fwd >>= fun (`answer( fwd_resp, tmp)) -> _put tmp >>= fun () ->
            let fwd_answer = Dns.Query.answer_of_response fwd_resp in
            S.disconnect fwd >>= fun tmp -> _put tmp >>= fun () ->
            Monad.return fwd_answer
         end
      | _ ->
         S.request fwd (fun x->x#dummy) () fd_fwd >>= fun tmp -> _put tmp >>= fun () -> (* no effect *)
         S.disconnect fwd >>= fun tmp -> _put tmp >>= fun () -> (* no effect *)
         Monad.return nxdomain
      end >>= fun answer ->
      let resp = Dns.Query.response_of_answer query answer in
      S.send cli (fun x->x#answer) resp >>= fun tmp -> _put tmp >>= fun () ->
      S.disconnect cli >>= fun tmp -> _put tmp >>= fun () ->
      S.close ()
      end) (fun _ ->
        loop ())
    in
    loop ()

let main () =
  let (>>=) = Lwt.(>>=) in
  Dns_server_unix.bind_fd ~address:"127.0.0.1" ~port:53
  >>= fun (fd_cli, _src) ->
  Dns_server_unix.bind_fd ~address:myip ~port:myport
  >>= fun (fd_fwd, _src) ->
  server fd_cli fd_fwd

let () =
  Random.self_init ();
  Lwt_main.run (main ())
