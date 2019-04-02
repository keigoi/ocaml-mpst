(* simple ring protocol *)
open Mpst_simple

let ring = (a --> b) msg @@ (b --> c) msg @@ (c --> a) msg finish3

(* the above is equivalent to following: *)
module ChVecExample = struct
  let force = Lazy.force

  let ring () =
    let ch0 = Event.new_channel ()
    and ch1 = Event.new_channel ()
    and ch2 = Event.new_channel ()
    in
    let rec ea0 =
      lazy (object method role_B =
                object method msg v =
                    Event.sync (Event.send ch0 v);
                    force ea1
                end
            end)
    and ea1 =
      lazy (Event.wrap (Event.receive ch2)
              (fun v -> `role_C(`msg(v, Close))))
    in
    let rec eb0 =
      lazy (Event.wrap (Event.receive ch0)
              (fun v -> `role_A(`msg(v, force eb1))))
    and eb1 =
      lazy (object method role_C =
                object method msg v =
                    Event.sync (Event.send ch1 v);
                    Close
                end
            end)
    in
    let rec ec0 =
      lazy (Event.wrap (Event.receive ch1)
              (fun v -> `role_B(`msg(v, force ec1))))
    and ec1 =
      lazy (object method role_A =
                object method msg v =
                    Event.sync (Event.send ch2 v);
                    Close
                end
            end)
    in
    let ea = force ea0 and eb = force eb0 and ec = force ec0
    in
    Cons(WrapSend(ea), Cons(WrapRecv(eb), Cons(WrapRecv(ec), Nil)))

end
(* let ring = ChVecExample.ring () *)

let ea = get_ep a ring
and eb = get_ep b ring
and ec = get_ep c ring

let tA = Thread.create (fun () ->
  let ea = ea#role_B#msg () in
  let `role_C(`msg((), ea)) = Event.sync ea in
  print_endline "A done";
  close ea) ()

(* let tA_bad (_:Obj.t) = Thread.create (fun () ->
 *   let `role_C(`msg((), ea)) = Event.sync ea in
 *   let ea = ea#role_B#msg () in
 *   print_endline "A done";
 *   close ea) () *)

let tB = Thread.create (fun () ->
  let `role_A(`msg((), eb)) = Event.sync eb in
  let eb = eb#role_C#msg () in
  print_endline "B done";
  close eb) ()

let tC = Thread.create (fun () ->
  let `role_B(`msg((), ec)) = Event.sync ec in
  let ec = ec#role_A#msg () in
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
      
let c =
  let g = test9 ()
  in
  let ea = get_ep a g in
  let eb = get_ep b g in
  let ec = get_ep c g in
  let ta = Thread.create (fun () ->
               let ea = ea#role_B#right () in
               let ea = ea#role_B#right () in
               let ea = ea#role_B#right () in
               let ea = ea#role_B#right () in
               let ea = ea#role_B#left () in
               let ea = ea#role_C#left () in
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
      
