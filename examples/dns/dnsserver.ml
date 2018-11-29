open Mpst.ThreeParty
open Lwt
open Dns
open Dnshelper

(* FIXME!!! *)
let myip = "172.21.24.215"
let myport = 12345
   
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
let cli = {a with role=Cli}
let srv = {b with role=Srv}
let otr = {c with role=Oth}


        
let dns () =
  ((cli,cli) -!-> (srv,srv)) query @@
  choice_req_at srv dummy_or_query
    (srv, ((srv,srv) -!-> (otr,otr)) dummy @@
          discon (srv,srv) (otr,otr) @@
          ((srv,srv) -?-> (cli,cli)) answer @@
          finish_)
    (srv, ((srv,srv) -!-> (otr,otr)) query @@
          ((otr,otr) -?-> (srv,srv)) answer @@
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
      Lwt_io.printf "DNS: %s returning NXDOMAIN\n" (to_string packet)
      >>= fun () ->
      Lwt.return None
    end
  | _ ->
    Lwt_io.printf "DNS: %s returning NXDOMAIN\n" (to_string packet)
    >>= fun () ->
    Lwt.return None

let sockaddr addr port =
  Lwt_unix.(ADDR_INET (Ipaddr_unix.to_inet_addr addr, port))

let server fd fd2 =
  let s = get_sess_ srv (dns ()) in
  let rec loop () =
    accept cli fd s >>= fun (`query((dst, query), s)) ->
    lookup query >>= fun answer ->
    begin match answer with
    | Some answer ->
       let s = request otr (fun x->x#dummy) () fd s in
       let s = disconnect otr s in
       Lwt.return (dst, query, answer, s)
    | None ->
       let fwd = sockaddr (Ipaddr.of_string_exn "8.8.8.8") 53 in
       let open Packet in
       match query.questions with
       | [] -> failwith "no question"
       | q::_ ->
       let query2 = Dns.Query.create ~id:12345 ~dnssec:false q.q_class q.q_type q.q_name in
       let s = request otr (fun x->x#query) (fwd,query2) fd2 s in
       receive otr s >>= fun (`answer((_dst,_query,answer),s)) ->
       let s = disconnect otr s in
       Lwt.return (dst,query,answer, s)
    end >>= fun (dst,query,answer,s) ->
    let s = send cli (fun x->x#answer) (dst,query,answer) s in
    let s = disconnect cli s in
    close s;
    loop ()
  in
  loop ()

let dummy_query =
    let open Packet in
    let detail = {qr=Query; opcode=Standard; aa=false; tc=false; rd=false; ra=false; rcode=NoError} in
    {id=0; detail; questions=[]; answers=[]; authorities=[]; additionals=[]}

let main () =
  Dns_server_unix.bind_fd ~address:"127.0.0.1" ~port:53
  >>= fun (fd, _src) ->
  Dns_server_unix.bind_fd ~address:myip ~port:myport
  >>= fun (fd2, _src) ->
  server {fd;query=dummy_query} {fd=fd2;query=dummy_query}

let () =
  Lwt_main.run (main ())
      
