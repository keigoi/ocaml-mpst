open Mpst.ThreeParty
open Lwt
open Dns
open Dnshelper

(*
  $ opam install dns-lwt-unix
  $ dune build --profile=release examples/dns/dnsserver.exe
  $ sudo _build/default/examples/dns/dnsserver.exe
  
  (in another window)
  $ host nagoya.my.domain 127.0.0.1
 *)

type cli = Cli
type srv = Srv
let cli = {a with role=Cli}
let srv = {b with role=Srv}

let dns () =
  ((cli,cli) -!-> (srv,srv)) query @@
  ((srv,srv) -?-> (cli,cli)) answer @@
  finish_

let addresses = [
  Dns.Name.of_string "nagoya.my.domain", Ipaddr.V4.of_string_exn "1.2.3.4";
  Dns.Name.of_string "gifu.my.domain", Ipaddr.V4.of_string_exn "1.2.3.5";
]

let nxdomain =
  Dns.Query.({ rcode = Dns.Packet.NXDomain; aa = true; answer = []; authority = []; additional = [] })

let dummy_query =
    let open Packet in
    let detail = {qr=Query; opcode=Standard; aa=false; tc=false; rd=false; ra=false; rcode=NoError} in
    {id=0; detail; questions=[]; answers=[]; authorities=[]; additionals=[]}

let lookup packet =
  let open Dns.Packet in
  match packet.questions with
  | [ { q_class = Q_IN; q_type = Q_A; q_name; _ } ] ->
    if List.mem_assoc q_name addresses then begin
      let ip = List.assoc q_name addresses in
      Lwt_io.printf "DNS: %s is a builtin: %s\n" (to_string packet) Ipaddr.V4.(to_string ip)
      >>= fun () ->
      let rrs = [ { name = q_name; cls = RR_IN; flush = false; ttl = 0l; rdata = A ip } ] in
      Lwt.return (Dns.Query.({ rcode = NoError; aa = true; answer = rrs; authority = []; additional = [] }))
    end else begin
      Lwt_io.printf "DNS: %s returning NXDOMAIN\n" (to_string packet)
      >>= fun () ->
      Lwt.return nxdomain
    end
  | _ ->
    Lwt_io.printf "DNS: %s returning NXDOMAIN\n" (to_string packet)
    >>= fun () ->
    Lwt.return nxdomain

let server fd =
  let s = get_sess_ srv (dns ()) in
  let rec loop () =
    accept cli fd s >>= fun (`query((dst, query), s)) ->
    lookup query >>= fun answer ->
    let s = send cli (fun x->x#answer) (dst, query, answer) s in
    let s = disconnect cli s in
    close s;
    loop ()
  in
  loop ()
    
let main () =
  Dns_server_unix.bind_fd ~address:"127.0.0.1" ~port:53
  >>= fun (fd, _src) ->
  server fd

let () =
  Lwt_main.run (main ())
      
