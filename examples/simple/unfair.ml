(* un-fairness *)
open Mpst_simple

let unfair () =
  let rec g =
    lazy begin
        choice_at a (to_b right_or_left)
          (a, (a --> b) right @@
              goto g)
          (a, (a --> b) left @@
              (a --> c) left @@
              finish)
      end
  in
  Lazy.force g

let () =
  let g = unfair ()
  in
  let ea = get_ep a g in
  let eb = get_ep b g in
  let _ = get_ep d g in
  print_endline"project on c";
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
                 match Event.sync (eb#role_A) with
                 | `right(_,eb) ->
                    print_endline "B: right";
                    loop eb
                 | `left(_,eb) ->
                    print_endline "B: left";
                    close eb
               in
               loop eb) ()
  and tc = Thread.create (fun () ->
               let `left(_,ec) = Event.sync (ec#role_A) in
               print_endline "C: closing";
               close ec) ()
  in
  List.iter Thread.join [ta; tb; tc];
  ()
