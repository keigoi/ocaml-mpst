(* simple ring protocol *)
open Concur_shims
open Mpst
open Mpst.Util

let ( let* ) = IO.bind
let () = print_endline "start"
let ring = gen @@ (a --> b) msg @@ (b --> c) msg @@ (c --> a) msg finish
let () = print_endline "global combinator finished"
let ea = get_ch a ring

let () = print_endline "EPP A finished"
and eb = get_ch b ring

let () = print_endline "EPP B finished"
and ec = get_ch c ring

let () = print_endline "EPP C finished"

let tA =
  Thread.create
    (fun () ->
      print_endline "A start";
      let* ea = send ea#role_B#msg () in
      let* (`msg ((), ea)) = receive ea#role_C in
      print_endline "A done";
      close ea)
    ()

(* let tA_bad (_:Obj.t) = Thread.create (fun () ->
 *   let `role_C(`msg((), ea)) = receive ea in
 *   let ea = ea#role_B#msg () in
 *   print_endline "A done";
 *   close ea) () *)

let tB =
  Thread.create
    (fun () ->
      print_endline "B start";
      let* (`msg ((), eb)) = receive eb#role_A in
      let* eb = send eb#role_C#msg () in
      print_endline "B done";
      close eb)
    ()

let tC =
  Thread.create
    (fun () ->
      print_endline "C start";
      let* (`msg ((), ec)) = receive ec#role_B in
      let* ec = send ec#role_A#msg () in
      print_endline "C done";
      close ec)
    ()

let () = IO.main_run @@ IO_list.iter Thread.join [ tA; tB; tC ]

(* incompatible branching at C between reception and closing *)
(* let test =
 *   choice_at a (to_b left_or_right)
 *     (a, (a --> b) left @@ (a --> c) left @@ finish)
 *     (a, (a --> b) right @@ finish) *)

(* incompatible branching at C after receiving msg from B (statically detected) *)
(* let test2 =
 *   choice_at a (to_b left_or_right)
 *     (a, (a --> b) left @@ (b --> c) msg @@ (c --> a) msg @@ finish)
 *     (a, (a --> b) right @@ (b --> c) msg @@ finish) *)

(* incompatible branching at C after sending msg to A (statically detected) *)
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

(* sending from a non-enabled role (statically detected) *)
(* let test8 =
 *   choice_at a (to_b left_or_right)
 *   (a, (a --> b) left  @@ (c --> b) left  @@ finish3)
 *   (a, (a --> b) right @@ (c --> b) right @@ finish3) *)
