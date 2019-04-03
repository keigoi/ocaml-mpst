(* simple ring protocol *)
open Mpst_simple

let ring =
  let rec g =
    lazy begin
        (c --> b) msg @@
        choice_at a (to_b left_or_right)
          (a, (a --> b) left @@
              (c --> b) msg @@
              (b --> c) left @@ goto3 g)
          (a, (a --> b) right @@
              (c --> b) msg @@
              (b --> c) right @@ finish3)
      end
  in
  (a --> c) msg @@
  Lazy.force g

let ea = get_ep a ring
and eb = get_ep b ring
and ec = get_ep c ring
    
let tA = Thread.create (fun () ->
  print_endline "A start";
   let ea = send (ea#role_C#msg) () in
  let rec loop ea =
    if Random.bool () then begin
      let ea = send (ea#role_B#left) () in
      loop ea
    end else begin
      let ea = send (ea#role_B#right) () in
      print_endline "A done";
      close ea
      end
  in
  loop ea) ()

let tB = Thread.create (fun () ->
  print_endline "B start";
  let rec loop eb =
  let `role_C(`msg((), eb)) = receive eb in
  match receive eb with
  | `role_A(`left((), eb)) ->
     print_endline "B left";
     let `role_C(`msg((), eb)) = receive eb in
     let eb = send (eb#role_C#left) () in
     loop eb
  | `role_A(`right((), eb)) ->
     print_endline "B right";
     let `role_C(`msg((), eb)) = receive eb in
     let eb = send (eb#role_C#right) () in
     close eb;
     print_endline "B done"
  in
  loop eb) ()

let tC = Thread.create (fun () ->
  print_endline "C start";
  let `role_A(`msg((), ec)) = receive ec in
  let rec loop ec =
  let ec = send (ec#role_B#msg) () in
  let ec = send (ec#role_B#msg) () in
  match receive ec with
  | `role_B(`left((), ec)) ->
     print_endline "C left";
     loop ec
  | `role_B(`right((), ec)) ->
     print_endline "C right";
     close ec;
     print_endline "C done"
  in
  loop ec) ()

let () =
  Random.self_init ();
  List.iter Thread.join [tA; tB; tC]
