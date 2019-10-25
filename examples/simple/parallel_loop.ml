(* two loops running in parallel, using "unbalanced loops"  *)

open Mpst_lwt

let () = Random.self_init ()
let (let/) m f = Lwt.bind m (fun x -> Lwt.bind (Lwt_unix.sleep (Random.float 0.1)) (fun () -> f x))
let return = Lwt.return

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
  let debug s = Lwt_io.printl ("a) " ^ s) in
  let (ch:'c) = get_ch a g in
  let rec loop (ea:'c) i =
    if i = 0 then
      let/ () = debug "sending right" in
      let/ ea = send ea#role_B#right () in
      let/ () = debug "closing ====" in
      close ea
    else
      let/ () = debug ("sending left, " ^ string_of_int i) in
      let/ ea = send ea#role_B#left i in
      loop ea (i-1)
  in
  loop ch i

let tb () =
  let debug s = Lwt_io.printl ("b) " ^ s) in
  let rec loop (eb:'c) =
    let/ () = debug "receive" in
    let/ lab = receive eb#role_A in
    match lab with
    | `left(i, eb) ->
       let/ () = debug ("matched left, "^string_of_int i) in
       loop eb
    | `right((), eb) ->
       let/ () = debug "matched right" in
       let/ eb = send eb#role_C#msg "heippa" in
       let/ () = debug "closing ====" in
       close eb
  in
  let (ch:'c) = get_ch b g in
  loop ch

let tc i =
  let debug s = Lwt_io.printl ("c) " ^ s) in
  let (ch:'c) = get_ch c g in
  let rec loop (ec:'c) i =
    if i = 0 then
      let/ () = debug "sending right" in
      let/ ec = send ec#role_D#right () in
      let/ () = debug "receiving last word from B" in
      let/ `msg(str,ec) = receive ec#role_B in
      let/ () = debug ("got: "^str) in
      let/ () = debug "closing ====" in
      close ec
    else
      let/ () = debug "sending left" in
      let/ ec = send ec#role_D#left i in
      loop ec (i-1)
  in
  loop ch i
    
let td ed =
  let debug s = Lwt_io.printl ("d) " ^ s) in
  let (ch:'c) = get_ch d g in
  let/ () = debug "receiving" in
  let rec loop (ed:'c) =
    let/ lab = receive ed#role_C in
    match lab with
    | `left(i, ed) ->
       let/ () = debug ("matched left, " ^ string_of_int i) in
       loop ed
    | `right((), ed) ->
       let/ () = debug "matched right" in
       let/ () = debug "closing ====" in
       close ed
  in
  loop ch

let () =
  Lwt_main.run (Lwt.join [ta 8; tb (); tc 5; td ()])
