(* simple ring protocol *)
open Mpst_simple

let () = print_endline "start"
let ring = (a --> b) msg @@ (b --> c) msg @@ (c --> a) msg finish3
let () = print_endline "global combinator finished"

let ea = get_ep a ring
let () = print_endline "EPP A finished"
and eb = get_ep b ring
let () = print_endline "EPP B finished"
and ec = get_ep c ring
let () = print_endline "EPP C finished"

let tA = Thread.create (fun () ->
  print_endline "A start";
  let ea = send (ea#role_B#msg) () in
  let `role_C(`msg((), ea)) = receive ea in
  print_endline "A done";
  close ea) ()

(* let tA_bad (_:Obj.t) = Thread.create (fun () ->
 *   let `role_C(`msg((), ea)) = Event.sync ea in
 *   let ea = ea#role_B#msg () in
 *   print_endline "A done";
 *   close ea) () *)

let tB = Thread.create (fun () ->
             print_endline "B start";
             let `role_A(`msg((), eb)) = Event.sync eb in
             let eb = send (eb#role_C#msg) () in
             print_endline "B done";
             close eb) ()

let tC = Thread.create (fun () ->
             print_endline "C start";
             let `role_B(`msg((), ec)) = Event.sync ec in
             let ec = send (ec#role_A#msg) () in
             print_endline "C done";
             close ec) ()

let () = List.iter Thread.join [tA; tB; tC]

(* let test =
 *   choice_at a (to_b left_or_right)
 *     (a, (a --> b) left @@ (a --> c) left @@ finish)
 *     (a, (a --> b) right @@ finish) *)

(* let test2 =
 *   choice_at a (to_b left_or_right)
 *     (a, (a --> b) left @@ (b --> c) msg @@ (c --> a) msg @@ finish)
 *     (a, (a --> b) right @@ (b --> c) msg @@ finish) *)

(* let test3 =
 *   choice_at a (to_b left_or_right)
 *     (a, (a --> b) left  @@ (b --> c) msg @@ (c --> a) msg @@ (c --> b) msg @@ finish)
 *     (a, (a --> b) right @@ (b --> c) msg @@ (c --> a) msg @@ finish) *)

(* receive from multiple roles *)
(* let rec g = lazy (\* will be a type error *\)
 *   (choice_at a b_or_c
 *    (a, (a --> b) left @@ (b --> c) left @@ goto g)
 *    (a, (a --> c) right @@ (c --> b) right @@ goto g)) *)

(* object merging failure *)
(* let test4 =
 *   choice_at a (to_b left_or_right)
 *   (a, (a --> b) left @@ finish)
 *   (a, (a --> b) left @@ finish) *)

(* let test5 =
 *   let rec g =
 *     lazy (choice_at a (to_b left_or_right)
 *             (a, goto g)
 *             (a, goto g))
 *   in
 *   let _ = Lazy.force g in (\* Fatal error: exception CamlinternalLazy.Undefined *\)
 *   () *)

(* let test6 =
 *   let rec g =
 *     lazy (choice_at a (to_b left_or_right)
 *             (a, (a --> b) left @@ goto g)
 *             (a, goto g))
 *   in
 *   let _ = Lazy.force g in (\* Fatal error: exception CamlinternalLazy.Undefined *\)
 *   () *)

(* let test7 =
 *   let rec g =
 *     lazy (choice_at a (to_b left_or_right)
 *             (a, goto g)
 *             (a, (a --> b) right @@ goto g))
 *   in
 *   let _ = Lazy.force g in (\* Fatal error: exception CamlinternalLazy.Undefined *\)
 *   () *)

(* sending from a non-enabled role (statically detected) *)
(* let test8 =
 *   choice_at a (to_b left_or_right)
 *   (a, (a --> b) left  @@ (c --> b) left  @@ finish)
 *   (a, (a --> b) right @@ (c --> b) right @@ finish) *)

(* sending from a non-enabled role (dynamically detected) *)
(* let test8 =
 *   choice_at a (to_b left_or_right)
 *   (a, (a --> b) left  @@ (c --> b) msg @@ finish)
 *   (a, (a --> b) right @@ (c --> b) msg @@ finish) *)

(* let finish = one @@ one @@ one @@ one @@ nil
 * let d = {label={make_obj=(fun v->object method role_D=v end);
 *                make_var=(fun v->(`role_D(v):[`role_D of _]))}; (\* explicit annotataion is mandatory *\)
 *          lens=Succ (Succ (Succ Zero))} *)

let test9 () =
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
  let g = test9 ()
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

let test10 =
  let rec bogus = lazy (goto2 bogus) in
  let g =
    (a --> b) msg @@
      Lazy.force bogus
  in
  let ea = get_ep a g
  and eb = get_ep b g
  in
  let _ : Thread.t =
    Thread.create (fun () ->
        print_endline "thread a";
        begin try
            ignore (send (ea#role_B#msg) ())
          with UngardedLoop ->
            print_endline "ungardedloop occurred as expected"
        end
      ) ()
  and () =
    begin
      try
        ignore (Event.sync eb)
      with
        UngardedLoop ->
        print_endline "ungardedloop occurred as expected"
    end
    
  in
  ()

  
                          
  
