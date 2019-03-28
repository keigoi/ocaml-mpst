(* simple ring protocol *)
open Mpst_simple

let ring = (a --> b) msg @@ (b --> c) msg @@ (c --> a) msg finish

let ea = get_ep a ring
and eb = get_ep b ring
and ec = get_ep c ring

let tA = Thread.create (fun () ->
  let ea = ea#role_b#msg () in
  let `role_c(`msg((), ea)) = Event.sync ea in
  print_endline "A done";
  close ea) ()

(* let tA_bad (_:Obj.t) = Thread.create (fun () ->
 *   let `role_c(`msg((), ea)) = Event.sync ea in
 *   let ea = ea#role_b#msg () in
 *   print_endline "A done";
 *   close ea) () *)

let tB = Thread.create (fun () ->
  let `role_a(`msg((), eb)) = Event.sync eb in
  let eb = eb#role_c#msg () in
  print_endline "B done";
  close eb) ()

let tC = Thread.create (fun () ->
  let `role_b(`msg((), ec)) = Event.sync ec in
  let ec = ec#role_a#msg () in
  print_endline "C done";
  close ec) ()

let () = List.iter Thread.join [tA; tB; tC]
