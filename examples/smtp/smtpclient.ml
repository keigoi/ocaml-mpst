open Mpst_plug
open Smtphelper

let s =
  Role
    {
      role_label =
        {
          make_obj =
            (fun v ->
              object
                method role_S = v
              end);
          call_obj = (fun o -> o#role_S);
        };
      role_index = Succ Zero;
      role_index0 = Succ Zero;
    }

let to_s m = to_ m s s s

let mail () =
  fix (fun z1 ->
      choice_at c (to_s mail_or_quit)
        ( c,
          (c --> s) mail
          @@ choice_at s (to_c _5xy_or_250)
               (s, (s --> c) _5xy z1)
               ( s,
                 (s --> c) _250
                 @@ fix (fun z2 ->
                        choice_at c (to_s rcpt_or_data)
                          (c, (c --> s) rcpt @@ (s --> c) _250 z2)
                          ( c,
                            (c --> s) data
                            @@ (s --> c) _354
                            @@ (c --> s) mailbody
                            @@ (s --> c) _250 z1 )) ) )
        (c, (c -?-> s) quit @@ finish))

let ehlo () =
  choice_at c (to_s ehlo_or_quit)
    (c, (c --> s) ehlo @@ (s --> c) _250 @@ mail ())
    (c, (c -?-> s) quit finish)

let smtp () = (s -!-> c) _220 @@ ehlo ()

let client hostport =
  let s = get_ch c (smtp ()) in
  let* conn = connect hostport in
  print_endline @@ "== connected to: " ^ hostport;
  let* (`_220 (reply, s)) = receive (s conn)#role_S in
  List.iter print_endline reply;
  let* s = s#role_S#ehlo "example.ocaml-mpst.net" in
  let* (`_250 (reply, s)) = receive s#role_S in
  List.iter print_endline reply;
  let rec mailloop s =
    print_endline "== Sender e-mail address:";
    let from = read_line () in
    let* s = s#role_S#mail from in
    let* var = receive s#role_S in
    match var with
    | `_5xy (reply, s) ->
        print_endline "== Error";
        List.iter print_endline reply;
        mailloop s
    | `_250 (reply, s) ->
        List.iter print_endline reply;
        Lwt.return s
  and rcptloop s =
    print_endline "== Recipient e-mail address (type '.' (dot) to finish):";
    let to_ = read_line () in
    if to_ = "." then Lwt.return s
    else
      let* s = s#role_S#rcpt to_ in
      let* var = receive s#role_S in
      match var with
      | `_5xy (reply, s) ->
          print_endline "== Error";
          List.iter print_endline reply;
          rcptloop s
      | `_250 (reply, s) ->
          List.iter print_endline reply;
          rcptloop s
  and mainloop s =
    let* s = mailloop s in
    let* s = rcptloop s in
    let* s = s#role_S#data "" in
    let* (`_354 (_reply, s)) = receive s#role_S in
    print_endline "== Single-line mail content:";
    let content = read_line () in
    let* s = s#role_S#mailbody [ content ] in
    let* (`_250 (reply, s)) = receive s#role_S in
    List.iter print_endline reply;
    print_endline "== Another email (type 'yes' to continue)?";
    if read_line () = "yes" then mainloop s
    else
      let* s = s#role_S#quit "" in
      close s
  in
  mainloop s

let () = Lwt_main.run (client Sys.argv.(1))
