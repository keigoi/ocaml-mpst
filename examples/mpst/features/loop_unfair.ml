(* un-fairness *)
open Concur_shims
open Mpst
open Mpst.Util

let ( let* ) = IO.bind

let unfair () =
  let g =
    gen
    @@ fix (fun t ->
           choice_at a (to_b right_or_left)
             (a, (a --> b) right @@ t)
             (a, (a --> b) left @@ (a --> c) left @@ finish))
  in
  g

let () =
  let g = unfair () in
  let ea = get_ch a g in
  print_endline "projected on a";
  let eb = get_ch b g in
  print_endline "projected on b";
  let ec = get_ch c g in
  print_endline "projected on c";
  let _ = get_ch d g in
  print_endline "projected on d";
  let ta =
    Thread.create
      (fun () ->
        let* ea = send ea#role_B#right () in
        let* ea = send ea#role_B#right () in
        let* ea = send ea#role_B#right () in
        let* ea = send ea#role_B#right () in
        let* ea = send ea#role_B#left () in
        let* ea = send ea#role_C#left () in
        close ea)
      ()
  and tb =
    Thread.create
      (fun () ->
        let rec loop eb =
          let* var = receive eb#role_A in
          match var with
          | `right (_, eb) ->
              print_endline "B: right";
              loop eb
          | `left (_, eb) ->
              print_endline "B: left";
              close eb
        in
        loop eb)
      ()
  and tc =
    Thread.create
      (fun () ->
        let* (`left (_, ec)) = receive ec#role_A in
        print_endline "C: closing";
        close ec)
      ()
  in
  IO.main_run @@ IO_list.iter Thread.join [ ta; tb; tc ]
