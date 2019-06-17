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
  ignore @@ Thread.create (fun () ->
                let _ =
                  match receive eb#role_A with
                  | `left((), eb) -> eb
                  | `right((), eb) -> eb
                in
                print_endline "receive successful";
                mustfail "bra:epb" (fun () -> receive eb#role_A)) ();
  mustfail "bra:epa" (fun () ->
    let mid = ea#role_B in
    (* check twice call of a role label method  *)
    mustfail "bra:epa" (fun () -> ea#role_B);
    let b = mid#left in
    (* check double use of a label *)
    mustfail "bra:epa1" (fun () -> mid#left);
    ignore @@ send b ();
    (* check double use of a bare channel *)
    mustfail "bra:epa0" (fun () -> send b ());
    (* check other labe is actually invalidated *)
    mustfail "bra:epa2" (fun () -> mid#right);
    ())
      
