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
  let shot = (a --> b) msg @@ finish in
  let ea = get_ep a shot in
  let eb = get_ep b shot in
  ignore @@ Thread.create (fun () ->
                let `msg(_,_) = receive eb#role_A in
                mustfail "shot:epb" (fun () -> receive eb#role_A)) ();
  let _ = send ea#role_B#msg () in
  mustfail "shot:epa" (fun () -> send ea#role_B#msg ())

let () =
  let bra =
    choice_at a (to_b left_or_right)
      (a, (a --> b) left @@ finish)
      (a, (a --> b) right @@ finish)
  in
  let ea = get_ep a bra in
  let eb = get_ep b bra in
  let t = Thread.create (fun () ->
                let mid = ea#role_B in
                (* check twice call of a role label method  *)
                mustfail "bra:epa" (fun () -> ea#role_B);
                let b = mid#left in
                (* check double use of a label *)
                mustfail "bra:epa0" (fun () -> mid#left);
                ignore @@ send b ();
                (* check double use of a bare channel *)
                mustfail "bra:epa1" (fun () -> send b ());
                (* check other label is actually invalidated *)
                mustfail "bra:epa2" (fun () -> mid#right);
            )()
  in
  let b = eb#role_A in
  mustfail "bra:epb0" (fun () -> eb#role_A);
  let _ =
    match receive b with
    | `left((), eb) -> eb
    | `right((), eb) -> eb
  in
  print_endline "receive successful";
  mustfail "bra:epb1" (fun () -> receive b);
  Thread.join t;
  ()
      
