(* dynamic linearity (affinity) checking *)
open Mpst_simple

let () = print_endline "dynamic linearity checking"

let mustfail name f =
  try
    f ();
    failwith (name^":no exception (unexpected)")
  with
    Mpst_common.LinFlag.InvalidEndpoint ->
    print_endline (name^":exception correctly occurred")
    
       
let () =
  let shot = unseq @@ (a --> b) msg @@ finish in
  let ea = get_ep a shot in
  let eb = get_ep b shot in
  let t = Thread.create (fun () ->
                let `msg(_,_) = receive eb#role_A in
                mustfail "shot:epb" (fun () -> receive eb#role_A)) ()
  in
  let _ = send ea#role_B#msg () in
  Thread.join t;
  mustfail "shot:epa" (fun () -> send ea#role_B#msg ())

let () =
  print_endline "test2 preparing";
  let bra =
    unseq @@
      choice_at a (to_b left_or_right)
      (a, (a --> b) left @@ finish)
      (a, (a --> b) right @@ finish)
  in
  let ea = get_ep a bra in
  let eb = get_ep b bra in
  print_endline "test2 start";
  let t = Thread.create (fun () ->
                let _ = send ea#role_B#left () in
                (* check twice call of a role label method  *)
                mustfail "bra:epa" (fun () -> send ea#role_B#right ());
            )()
  in
  let _ =
    match receive eb#role_A with
    | `left((), eb) -> eb
    | `right((), eb) -> eb
  in
  print_endline "receive successful";
  mustfail "bra:epb1" (fun () -> receive eb#role_A);
  Thread.join t;
  ()
      
