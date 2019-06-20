(* sending from a non-choosing role *)
open Mpst

   
let prot =
  gen @@
  (a --> c) msg @@
  fix (fun t ->
        (c --> b) msg @@
        choice_at a (to_b left_or_right)
          (a, (a --> b) left @@
              (c --> b) msg @@
              (b --> c) left @@ t)
          (a, (a --> b) right @@
              (c --> b) msg @@
              (b --> c) right @@ finish))

let () = print_endline "global defined"
let ea = get_ep a prot
let () = print_endline "EPP a done"
let eb = get_ep b prot
let () = print_endline "EPP b done"
let ec = get_ep c prot
let () = print_endline "EPP c done"
    
let tA = Thread.create (fun () ->
  print_endline "A start";
  let ea = send ea#role_C#msg () in
  let rec loop ea =
    if Random.bool () then begin
      let ea = send ea#role_B#left () in
      loop ea
    end else begin
      let ea = send ea#role_B#right () in
      print_endline "A done";
      close ea
      end
  in
  loop ea) ()

let tB = Thread.create (fun () ->
  print_endline "B start";
  let rec loop eb =
  let `msg((), eb) = receive (eb#role_C) in
  match receive eb#role_A with
  | `left((), eb) ->
     print_endline "B left";
     let `msg((), eb) = receive (eb#role_C) in
     let eb = send eb#role_C#left () in
     loop eb
  | `right((), eb) ->
     print_endline "B right";
     let `msg((), eb) = receive (eb#role_C) in
     let eb = send eb#role_C#right () in
     close eb;
     print_endline "B done"
  in
  loop eb) ()

let tC = Thread.create (fun () ->
  print_endline "C start";
  let `msg((), ec) = receive ec#role_A in
  let rec loop ec =
  let ec = send ec#role_B#msg () in
  let ec = send ec#role_B#msg () in
  match receive (ec#role_B) with
  | `left((), ec) ->
     print_endline "C left";
     loop ec
  | `right((), ec) ->
     print_endline "C right";
     close ec;
     print_endline "C done"
  in
  loop ec) ()

let () =
  Random.self_init ();
  List.iter Thread.join [tA; tB; tC]