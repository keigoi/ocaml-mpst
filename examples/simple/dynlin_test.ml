(* dynamic linearity (affinity) checking *)
open Concur_shims
open Mpst
open Mpst.Util

let (let*) = IO.bind

let () = print_endline "dynamic linearity checking"

let mustfail name f =
  IO.catch begin fun () ->
    print_endline @@ name^":trying";
    let* _ = f () in
    IO.return @@ failwith (name^":no exception (unexpected)")
      end begin function
      | InvalidEndpoint ->
        IO.printl (name^":exception correctly occurred")
      | exn -> Lwt.fail exn
    end
    
       
let (_ : unit IO.io) =
  let shot = gen @@ (a --> b) msg @@ finish in
  let ea = get_ch a shot in
  let eb = get_ch b shot in
  let t = Thread.create (fun () ->
                let* `msg(_,_) = receive eb#role_A in
                IO.return @@ mustfail "shot:epb" (fun () -> receive eb#role_A)) ()
  in
  let* _ = send ea#role_B#msg () in
  let* () = Thread.join t in
  mustfail "shot:epa" (fun () -> send ea#role_B#msg ())

let (_ : unit IO.io) =
  print_endline "test2 preparing";
  let bra =
    gen @@
      choice_at a (to_b left_or_right)
      (a, (a --> b) left @@ finish)
      (a, (a --> b) right @@ finish)
  in
  let ea = get_ch a bra in
  let eb = get_ch b bra in
  print_endline "test2 start";
  let t = Thread.create (fun () ->
                let _ = send ea#role_B#left () in
                (* check twice call of a role label method  *)
                mustfail "bra:epa" (fun () -> send ea#role_B#right ());
            )()
  in
  let* () =
    let* var = receive eb#role_A in
    match var with
    | `left((), _) -> IO.return ()
    | `right((), _) -> IO.return ()
  in
  let* () = IO.printl "receive successful" in
  let* () = mustfail "bra:epb1" (fun () -> receive eb#role_A) in
  Thread.join t
      
