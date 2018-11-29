module S = Mpst.Session
let (>>=) = Lwt.(>>=)

(* FIXME!!! *)
let myip = "10.0.1.11"
let myport = 12345

let forward_ip = "8.8.8.8"
let forward_port = 53

(*
  $ opam install dns-lwt-unix
  $ dune build --profile=release examples/dns/dnsserver.exe
  $ sudo _build/default/examples/dns/dnsserver.exe

  (in another window)
  $ host nagoya.my.domain 127.0.0.1
 *)

type cli = Cli
type srv = Srv
type oth = Oth
let cli = {Mpst.ThreeParty.a with role=Cli}
let srv = {Mpst.ThreeParty.b with role=Srv}
let fwd = {Mpst.ThreeParty.c with role=Oth}



let dns () =
  let open Mpst.ThreeParty in
  let open Dnshelper in
  ((cli,cli) -!-> (srv,srv)) query @@
  choice_req_at srv dummy_or_query
    (srv, ((srv,srv) -!-> (fwd,fwd)) dummy @@
          discon (srv,srv) (fwd,fwd) @@
          ((srv,srv) -?-> (cli,cli)) answer @@
          finish_)
    (srv, ((srv,srv) -!-> (fwd,fwd)) query @@
          ((fwd,fwd) -?-> (srv,srv)) answer @@
          ((srv,srv) -?-> (cli,cli)) answer @@
          finish_)

let addresses = [
  Dns.Name.of_string "nagoya.my.domain", Ipaddr.V4.of_string_exn "1.2.3.4";
  Dns.Name.of_string "gifu.my.domain", Ipaddr.V4.of_string_exn "1.2.3.5";
]

let nxdomain =
  Dns.Query.({ rcode = Dns.Packet.NXDomain; aa = true; answer = []; authority = []; additional = [] })

let lookup packet =
  let open Dns.Packet in
  match packet.questions with
  | [ { q_class = Q_IN; q_type = Q_A; q_name; _ } ] ->
    if List.mem_assoc q_name addresses then begin
      let ip = List.assoc q_name addresses in
      Lwt_io.printf "DNS: %s is a builtin: %s\n" (to_string packet) Ipaddr.V4.(to_string ip)
      >>= fun () ->
      let rrs = [ { name = q_name; cls = RR_IN; flush = false; ttl = 0l; rdata = A ip } ] in
      Lwt.return (Some (Dns.Query.({ rcode = NoError; aa = true; answer = rrs; authority = []; additional = [] })))
    end else begin
      Lwt.return None
    end
  | _ ->
    Lwt.return None

let sockaddr addr port =
  Lwt_unix.(ADDR_INET (Ipaddr_unix.to_inet_addr addr, port))

let server fd_cli fd_other =
  let s = Mpst.ThreeParty.get_sess_ srv (dns ()) in
  let rec loop () =
    S.accept cli fd_cli s >>= fun (`query((cli_addr, query), s)) ->
    lookup query >>= fun answer ->
    begin match answer with
    | Some answer ->
       let s = S.request fwd (fun x->x#dummy) () fd_cli s in (* no effect *)
       let s = S.disconnect fwd s in (* no effect *)
       Lwt.return (answer, s)
    | None ->
       match query.questions with
       | [] ->
          let s = S.request fwd (fun x->x#dummy) () fd_cli s in (* no effect *)
          let s = S.disconnect fwd s in (* no effect *)
          Lwt.return (nxdomain, s)
       | q::_ -> (* FIXME: discarding rest of queries *)
          let fwd_query =
            Dns.Query.create
              ~id:(Random.int (1 lsl 16))
              ~dnssec:false
              q.q_class q.q_type q.q_name
          in
          let fwd_addr = sockaddr (Ipaddr.of_string_exn forward_ip) forward_port in
          let s = S.request fwd (fun x->x#query) (fwd_addr, fwd_query) fd_other s in
          S.receive fwd s >>= fun (`answer((_dst, fwd_resp),s)) ->
          let fwd_answer = Dns.Query.answer_of_response fwd_resp in
          let s = S.disconnect fwd s in
          Lwt.return (fwd_answer, s)
    end >>= fun (answer, s) ->
    let resp = Dns.Query.response_of_answer query answer in
    let s = S.send cli (fun x->x#answer) (cli_addr, resp) s in
    let s = S.disconnect cli s in
    S.close s;
    loop ()
  in
  loop ()

let main () =
  Dns_server_unix.bind_fd ~address:"127.0.0.1" ~port:53
  >>= fun (fd_cli, _src) ->
  Dns_server_unix.bind_fd ~address:myip ~port:myport
  >>= fun (fd_fwd, _src) ->
  server fd_cli fd_fwd

let () =
  Random.self_init ();
  Lwt_main.run (main ())
