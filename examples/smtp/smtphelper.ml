open Mpst_plug
let (let*) = Lwt.bind

let connect hostport =
  let host,port =
    match Str.split (Str.regexp ":") hostport with
    | [host] -> host, "25"
    | host::port::_ -> host, port

    | [] -> assert false
  in
  let* addrs = Lwt_unix.getaddrinfo host port [] in
  match addrs with
  | [] -> failwith ("Host not found " ^ host)
  | h::_ -> Lwt_io.open_connection h.Unix.ai_addr

let rec read_fully in_ch lines =
  let* line = Lwt_io.read_line in_ch in
  if String.sub line 3 1 = "-" then
    read_fully in_ch (line::lines)
  else
    Lwt.return (List.rev (line::lines))

type buf = string list option

let read_reply code (in_ch,_) (buf:buf) =
  let* lines =
    match buf with
    | Some x -> Lwt.return x
    | None -> read_fully in_ch []
  in
  match lines with
  | [] -> assert false
  | (first::_) ->
     if String.sub first 0 (String.length code) = code then begin
         Lwt.return (None, Some lines)
       end else begin
         Lwt.return (Some lines, None)
       end

let write_command cmd (_,out_ch) line =
  Lwt_io.write out_ch (cmd ^ " " ^ line ^ "\r\n")

let write_mailbody (_,out_ch) mail =
  Lwt_io.write out_ch (String.concat "\r\n" mail ^ "\r\n.\r\n")

let _220 =
  Slabel
    {label=
       {obj={make_obj=(fun f -> object method _220=f end);
             call_obj=(fun o -> o#_220)};
        var=(fun v -> `_220(v))};
     handler={write=(fun _ _ -> failwith "not implemented");
              read=(fun x -> read_reply "220" x);
    }}

let _235 =
  Slabel
    {label=
       {obj={make_obj=(fun f -> object method _235=f end);
             call_obj=(fun o -> o#_235)};
        var=(fun v -> `_235(v))};
     handler={write=(fun _ _ -> failwith "not implemented");
              read=(fun x -> read_reply "235" x);
    }}

let _250 =
  Slabel
    {label=
       {obj={make_obj=(fun f -> object method _250=f end);
             call_obj=(fun o -> o#_250)};
        var=(fun v -> `_250(v))};
     handler={write=(fun _ _ -> failwith "not implemented");
              read=(fun x -> read_reply "250" x);
    }}

let _354 =
  Slabel
    {label=
       {obj={make_obj=(fun f -> object method _354=f end);
             call_obj=(fun o -> o#_354)};
        var=(fun v -> `_354(v))};
     handler={write=(fun _ _ -> failwith "not implemented");
              read=(fun x -> read_reply "354" x);
    }}

let _5xy =
  Slabel
    {label=
       {obj={make_obj=(fun f -> object method _5xy=f end);
             call_obj=(fun o -> o#_5xy)};
        var=(fun v -> `_5xy(v))};
     handler={write=(fun _ _ -> failwith "not implemented");
              read=(fun x -> read_reply "5" x);
    }}

let ehlo =
  Slabel
    {label=
       {obj={make_obj=(fun f -> object method ehlo=f end);
             call_obj=(fun o -> o#ehlo)};
        var=(fun v -> `ehlo(v))};
     handler={write=(fun x -> write_command "EHLO" x);
              read=(fun _ _ -> failwith "not implemented");
    }}

let quit =
  Slabel
    {label=
       {obj={make_obj=(fun f -> object method quit=f end);
             call_obj=(fun o -> o#quit)};
        var=(fun v -> `quit(v))};
     handler={write=(fun x -> write_command "QUIT" x);
              read=(fun _ _ -> failwith "not implemented");
    }}

let mail =
  Slabel
    {label=
       {obj={make_obj=(fun f -> object method mail=f end);
             call_obj=(fun o -> o#mail)};
        var=(fun v -> `mail(v))};
     handler={write=(fun x -> write_command "MAIL FROM:" x);
              read=(fun _ _ -> failwith "not implemented");
    }}

let rcpt =
  Slabel
    {label=
       {obj={make_obj=(fun f -> object method rcpt=f end);
             call_obj=(fun o -> o#rcpt)};
        var=(fun v -> `rcpt(v))};
     handler={write=(fun x -> write_command "RCPT TO:" x);
              read=(fun _ _ -> failwith "not implemented");
    }}

let data =
  Slabel
    {label=
       {obj={make_obj=(fun f -> object method data=f end);
             call_obj=(fun o -> o#data)};
        var=(fun v -> `data(v))};
     handler={write=(fun x -> write_command "DATA" x);
              read=(fun _ _ -> failwith "not implemented");
    }}

let mailbody =
  Slabel
    {label=
       {obj={make_obj=(fun f -> object method mailbody=f end);
             call_obj=(fun o -> o#mailbody)};
        var=(fun v -> `mailbody(v))};
     handler={write=write_mailbody;
              read=(fun _ _ -> failwith "not implemented");
    }}

let ehlo_or_quit =
  {disj_merge=(fun l r -> object method ehlo=l#ehlo method quit=r#quit end);
   disj_splitL=(fun lr -> (lr :> <ehlo : _>));
   disj_splitR=(fun lr -> (lr :> <quit : _>));
  }

let mail_or_quit =
  {disj_merge=(fun l r -> object method mail=l#mail method quit=r#quit end);
   disj_splitL=(fun lr -> (lr :> <mail : _>));
   disj_splitR=(fun lr -> (lr :> <quit : _>));
  }

let _5xy_or_250 =
  {disj_merge=(fun l r -> object method _5xy=l#_5xy method _250=r#_250 end);
   disj_splitL=(fun lr -> (lr :> <_5xy : _>));
   disj_splitR=(fun lr -> (lr :> <_250 : _>));
  }

let rcpt_or_data =
  {disj_merge=(fun l r -> object method rcpt=l#rcpt method data=r#data end);
   disj_splitL=(fun lr -> (lr :> <rcpt : _>));
   disj_splitR=(fun lr -> (lr :> <data : _>));
  }

let _235_or_5xy =
  {disj_merge=(fun l r -> object method _235=l#_235 method _5xy=r#_5xy end);
   disj_splitL=(fun lr -> (lr :> <_235 : _>));
   disj_splitR=(fun lr -> (lr :> <_5xy : _>));
  }
