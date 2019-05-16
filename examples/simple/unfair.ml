(* un-fairness *)
open Mpst_simple
let a = {label={make_obj=(fun v->object method role_A=v end);
                call_obj=(fun o->o#role_A);
               make_var=(fun v->(`role_A(v):[`role_A of _]))}; (* explicit annotataion is mandatory *)
         lens=Zero}
let b = {label={make_obj=(fun v->object method role_B=v end);
                call_obj=(fun o->o#role_B);
               make_var=(fun v->(`role_B(v):[`role_B of _]))}; (* explicit annotataion is mandatory *)
         lens=Succ Zero}
let c = {label={make_obj=(fun v->object method role_C=v end);
                call_obj=(fun o->o#role_C);
               make_var=(fun v->(`role_C(v):[`role_C of _]))}; (* explicit annotataion is mandatory *)
         lens=Succ (Succ (Succ Zero))}
let d = {label={make_obj=(fun v->object method role_D=v end);
                call_obj=(fun o->o#role_D);
               make_var=(fun v->(`role_D(v):[`role_D of _]))}; (* explicit annotataion is mandatory *)
         lens=Succ (Succ Zero)}

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
