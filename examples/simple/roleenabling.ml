(* sending from a non-choosing role *)
open Mpst

let roleenabling =
  unseq @@
  choice_at a (to_b left_or_right)
    (a, (a --> b) left @@
        (c --> b) msg @@
        (b --> c) left @@ finish)
    (a, (a --> b) right @@
        (c --> b) msg @@
        (b --> c) right @@ finish)

let ea = get_ep a roleenabling
and eb = get_ep b roleenabling
and ec = get_ep c roleenabling
    
let tA = Thread.create (fun () ->
  print_endline "A start";
  let ea =
    if Random.bool () then
      send (ea#role_B#left) ()
    else
      send (ea#role_B#right) ()
  in
  print_endline "A done";
  close ea) ()

let tB = Thread.create (fun () ->
  print_endline "B start";
  match Event.sync (eb#role_A) with
  | `left((), eb) ->
     print_endline "B left";
     let `msg((), eb) = Event.sync (eb#role_C) in
     let eb = send (eb#role_C#left) () in
     close eb
  | `right((), eb) ->
     print_endline "B right";
     let `msg((), eb) = Event.sync (eb#role_C) in
     let eb = send (eb#role_C#right) () in
     close eb) ()

let tC = Thread.create (fun () ->
  print_endline "C start";
  let ec = send (ec#role_B#msg) () in
  match Event.sync (ec#role_B) with
  | `left((), ec) ->
     print_endline "C left";
     close ec;
     print_endline "C done"
  | `right((), ec) ->
     print_endline "C right";
     close ec;
     print_endline "C done") ()

let () =
  Random.self_init ();
  List.iter Thread.join [tA; tB; tC]
