(* two loops running in parallel, based on an "unbalanced choice"  *)

open Concur_shims
open Mpst
open Mpst.Util

let () = Random.self_init ()
let bind m f = IO.bind m (fun x -> IO.bind (Unix.sleepf (Random.float 0.1)) (fun () -> f x))
let (let*) = bind
let return = IO.return

(* A-B and C-D pairs are running in parallel, then B and C "joins" in the end. *)
let g =
  gen @@
  fix (fun t ->
  choice_at a (to_b left_or_right)
    (a, (a --> b) left @@ t)
    (a, (a --> b) right @@
    fix (fun t2 ->
      choice_at c (to_d left_or_right)
      (c, (c --> d) left t2)
      (c, (c --> d) right @@
          (b --> c) msg finish))))

let ta i =
  let debug s = IO.printl ("a) " ^ s) in
  let (ch:'c) = get_ch a g in
  let rec loop (ch:'c) i =
    if i = 0 then
      let* () = debug "sending right" in
      let* ch = send ch#role_B#right () in
      let* () = debug "closing ====" in
      close ch
    else
      let* () = debug ("sending left, " ^ string_of_int i) in
      let* ch = send ch#role_B#left i in
      loop ch (i-1)
  in
  loop ch i

let tb () =
  let debug s = IO.printl ("b) " ^ s) in
  let rec loop (ch:'c) =
    let* () = debug "receive" in
    let* lab = receive ch#role_A in
    match lab with
    | `left(i, ch) ->
       let* () = debug ("matched left, "^string_of_int i) in
       loop ch
    | `right((), ch) ->
       let* () = debug "matched right" in
       let* ch = send ch#role_C#msg "heippa" in
       let* () = debug "closing ====" in
       close ch
  in
  let (ch:'c) = get_ch b g in
  loop ch

let tc i =
  let debug s = IO.printl ("c) " ^ s) in
  let (ch:'c) = get_ch c g in
  let rec loop (ch:'c) i =
    if i = 0 then
      let* () = debug "sending right" in
      let* ch = send ch#role_D#right () in
      let* () = debug "receiving last word from B" in
      let* `msg(str,ch) = receive ch#role_B in
      let* () = debug ("got: "^str) in
      let* () = debug "closing ====" in
      close ch
    else
      let* () = debug "sending left" in
      let* ch = send ch#role_D#left i in
      loop ch (i-1)
  in
  loop ch i
    
let td () =
  let debug s = IO.printl ("d) " ^ s) in
  let (ch:'c) = get_ch d g in
  let* () = debug "receiving" in
  let rec loop (ch:'c) =
    let* lab = receive ch#role_C in
    match lab with
    | `left(i, ch) ->
       let* () = debug ("matched left, " ^ string_of_int i) in
       loop ch
    | `right((), ch) ->
       let* () = debug "matched right" in
       let* () = debug "closing ====" in
       close ch
  in
  loop ch

let () =
  IO.main_run (IO_list.iter Thread.join [Thread.create ta 8; Thread.create tb (); Thread.create tc 5; Thread.create td ()])
