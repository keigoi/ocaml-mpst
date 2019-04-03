(* simple ring protocol *)
open Mpst_simple

let ring =
  choice_at a (to_b left_or_right)
    (a, (a --> b) left @@
        (c --> b) msg @@
        (b --> c) left @@ finish3)
    (a, (a --> b) right @@
        (c --> b) msg @@
        (b --> c) right @@ finish3)

let ea = get_ep a ring
and eb = get_ep b ring
and ec = get_ep c ring
    
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
  match receive eb with
  | `role_A(`left((), eb)) ->
     print_endline "B left";
     let `role_C(`msg((), eb)) = receive eb in
     let eb = send (eb#role_C#left) () in
     close eb
  | `role_A(`right((), eb)) ->
     print_endline "B right";
     let `role_C(`msg((), eb)) = receive eb in
     let eb = send (eb#role_C#right) () in
     close eb) ()

let tC = Thread.create (fun () ->
  print_endline "C start";
  let ec = send (ec#role_B#msg) () in
  match receive ec with
  | `role_B(`left((), ec)) ->
     print_endline "C left";
     close ec;
     print_endline "C done"
  | `role_B(`right((), ec)) ->
     print_endline "C right";
     close ec;
     print_endline "C done") ()

let () =
  Random.self_init ();
  List.iter Thread.join [tA; tB; tC]
