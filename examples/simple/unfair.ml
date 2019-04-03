(* un-fairness *)
open Mpst_simple

let unfair () =
  let rec g =
    lazy begin
        choice_at a (to_b left_or_right)
          (a, (a --> b) left @@
              (a --> c) left @@
              finish3)
          (a, (a --> b) right @@
              goto3 g)
      end
  in
  Lazy.force g
  
let () =
  let g = unfair ()
  in
  let ea = get_ep a g in
  let eb = get_ep b g in
  let ec = get_ep c g in
  let ta = Thread.create (fun () ->
               let ea = send (ea#role_B#right) () in
               let ea = send (ea#role_B#right) () in
               let ea = send (ea#role_B#right) () in
               let ea = send (ea#role_B#right) () in
               let ea = send (ea#role_B#left) () in
               let ea = send (ea#role_C#left) () in
               close ea
             )()
  and tb = Thread.create (fun () ->
               let rec loop eb =
                 match Event.sync eb with
                 | `role_A(`right(_,eb)) ->
                    print_endline "B: right";
                    loop eb
                 | `role_A(`left(_,eb)) ->
                    print_endline "B: left";
                    close eb
               in
               loop eb) ()
  and tc = Thread.create (fun () ->
               let `role_A(`left(_,ec)) = Event.sync ec in
               print_endline "C: closing";
               close ec) ()
  in
  List.iter Thread.join [ta; tb; tc];
  ()
