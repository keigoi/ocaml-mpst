open Mpst_plug

let ( >>= ) = Lwt.( >>= )
let ( let/ ) = Lwt.bind

open Dnshelper

let myip = "0.0.0.0"
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

let dns () =
  (cli -!-> srv) query
  @@ (* DNS query from a client *)
  choice_req_at srv
    (to_fwd query_or_dummy) (* do I have an entry for the query?  *)
    ( srv,
      (srv -!-> fwd) query
      (* if not, forward the query to aoother server *)
      @@ (fwd -?-> srv) answer
      (* and receive a reply *)
      @@ (srv -?-> cli) answer
      @@ (* then send back it to the client *)
      finish )
    ( srv,
      (srv -!-> fwd) dummy
      (* DUMMY (no effect)  *)
      @@ disconnect srv fwd
      (* DUMMY (no effect) *)
      @@ (srv -?-> cli) answer
      @@ (* send back to the client the answer *)
      finish )

let addresses =
  [
    (Dns.Name.of_string "nagoya.my.domain", Ipaddr.V4.of_string_exn "1.2.3.4");
    (Dns.Name.of_string "gifu.my.domain", Ipaddr.V4.of_string_exn "1.2.3.5");
  ]

let nxdomain =
  Dns.Query.
    {
      rcode = Dns.Packet.NXDomain;
      aa = true;
      answer = [];
      authority = [];
      additional = [];
    }

let lookup ({ Dns.Packet.q_name; _ } as question) =
  if List.mem_assoc q_name addresses then
    let ip = List.assoc q_name addresses in
    Lwt_io.printf "DNS: %s is a builtin: %s\n"
      (Dns.Packet.question_to_string question)
      Ipaddr.V4.(to_string ip)
    >>= fun () ->
    let rrs =
      [
        {
          Dns.Packet.name = q_name;
          cls = RR_IN;
          flush = false;
          ttl = 0l;
          rdata = A ip;
        };
      ]
    in
    Lwt.return
      (Some
         Dns.Query.
           {
             rcode = NoError;
             aa = true;
             answer = rrs;
             authority = [];
             additional = [];
           })
  else Lwt.return None

let server (fd_cli : Lwt_unix.file_descr) (fd_fwd : Lwt_unix.file_descr) =
  let fwd_addr =
    Lwt_unix.(
      ADDR_INET
        ( Ipaddr_unix.to_inet_addr (Ipaddr.of_string_exn forward_ip),
          forward_port ))
  in
  let fd_fwd = create_resolver_fd ~fd:fd_fwd ~peer_addr:fwd_addr in
  let fd_cli = create_listener_fd ~fd:fd_cli in

  let rec loop () =
    (* get a new session endpoint *)
    let s = get_ch srv (dns ()) in

    let/ (`query (query, s)) = receive (s fd_cli)#role_Cli in

    (match query.questions with
    | ({ Dns.Packet.q_class = Q_IN; q_type = Q_A; _ } as question) :: _ -> (
        let/ answer = lookup question in

        match answer with
        | Some answer ->
            let/ s = (s fd_fwd)#role_Fwd#dummy () in
            (* no effect *)
            let/ s = s#disconnect in
            (* no effect *)
            Lwt.return (answer, s)
        | None ->
            let fwd_query =
              Dns.Query.create
                ~id:(Random.int (1 lsl 16))
                ~dnssec:false question.q_class question.q_type question.q_name
            in
            let/ s = (s fd_fwd)#role_Fwd#query fwd_query in
            let/ (`answer (fwd_resp, s)) = receive s#role_Fwd in
            let fwd_answer = Dns.Query.answer_of_response fwd_resp in
            Lwt.return (fwd_answer, s))
    | _ ->
        let/ s = (s fd_fwd)#role_Fwd#dummy () in
        (* no effect *)
        let/ s = s#disconnect in
        (* no effect *)
        Lwt.return (nxdomain, s))
    >>= fun (answer, s) ->
    let resp = Dns.Query.response_of_answer query answer in
    let/ s = s#role_Cli#answer resp in
    let/ () = close s in
    loop ()
  in
  loop ()

let main () =
  Dns_server_unix.bind_fd ~address:"127.0.0.1" ~port:53
  >>= fun (fd_cli, _src) ->
  Dns_server_unix.bind_fd ~address:myip ~port:myport >>= fun (fd_fwd, _src) ->
  server fd_cli fd_fwd

let () =
  Random.self_init ();
  Lwt_main.run (main ())
