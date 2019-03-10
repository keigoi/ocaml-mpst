open Mpst_explicit.Global
open Mpst_explicit.Session
module S = Mpst_explicit.Session
let (>>=) = Lwt.(>>=)
open Dnshelper

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
  $ host www.google.com 127.0.0.1
 *)

module Roles = struct
  type cli = Cli
  type srv = Srv
  type oth = Oth
end

let cli = {Mpst_explicit.Parties.a with role=Roles.Cli}
let srv = {Mpst_explicit.Parties.b with role=Roles.Srv}
let fwd = {Mpst_explicit.Parties.c with role=Roles.Oth}

let emp = Cons(lv Unit,lv@@Cons(lv Unit,lv@@Cons(lv Unit,lv Nil)))
let get_sess_ r c = Sess(emp, unprot @@ lens_get_ r.lens c)

let finish_ :
      (((unit * (unit * (unit * unit))) slots, close) prot *
         (((unit * (unit * (unit * unit))) slots, close) prot *
            (((unit * (unit * (unit * unit))) slots, close) prot * unit)))
        slots lazy_t =
  lv@@Cons(lv@@Prot Close,lv@@Cons(lv@@Prot Close,lv@@Cons(lv@@Prot Close,lv Nil)))

let dns () =
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

let server (fd_cli : Lwt_unix.file_descr) (fd_fwd : Lwt_unix.file_descr) =
  let fwd_addr =
    Lwt_unix.(ADDR_INET (Ipaddr_unix.to_inet_addr (Ipaddr.of_string_exn forward_ip),
                         forward_port))
  in
  let fd_fwd = create_resolver_fd ~fd:fd_fwd ~peer_addr:fwd_addr in
  let fd_cli = create_listener_fd ~fd:fd_cli in
  let s = get_sess_ srv (dns ()) in
  let rec loop () =
    S.accept cli fd_cli s >>= fun (`query(query, s)) ->
    begin match query.questions with
    | ({Dns.Packet.q_class = Q_IN; q_type = Q_A; _} as question) :: _ ->
       lookup question >>= fun answer ->
       begin match answer with
       | Some answer ->
          let s = S.request fwd (fun x->x#dummy) () fd_fwd s in (* no effect *)
          let s = S.disconnect fwd s in (* no effect *)
          Lwt.return (answer, s)
       | None ->
          let fwd_query =
            Dns.Query.create
              ~id:(Random.int (1 lsl 16))
              ~dnssec:false
              question.q_class question.q_type question.q_name
          in
          let s = S.request fwd (fun x->x#query) fwd_query fd_fwd s in
          S.receive fwd s >>= fun (`answer( fwd_resp,s)) ->
          let fwd_answer = Dns.Query.answer_of_response fwd_resp in
          let s = S.disconnect fwd s in
          Lwt.return (fwd_answer, s)
       end
    | _ ->
       let s = S.request fwd (fun x->x#dummy) () fd_fwd s in (* no effect *)
       let s = S.disconnect fwd s in (* no effect *)
       Lwt.return (nxdomain, s)
    end >>= fun (answer, s) ->
    let resp = Dns.Query.response_of_answer query answer in
    let s = S.send cli (fun x->x#answer) resp s in
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
