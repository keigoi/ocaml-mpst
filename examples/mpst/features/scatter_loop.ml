(* simple loop w/ multicasts *)
open Concur_shims
open Mpst
open Mpst.Util

let ( let* ) = IO.bind

let prot () =
  fix @@ fun t -> (gather b a) msg @@ (a --> c) msg @@ (scatter c b) msg @@ t

let rec tA (s : 't) i =
  let (_ : 't ty) = get_ty a (prot ()) in
  let* () = IO.printl "tA" in
  let* (`msg (_, s)) = receive_many s#role_B in
  let* s = send s#role_C#msg () in
  let* () = Unix.sleepf 0.1 in
  if i > 10 then exit 0 else tA s (i + 1)

let tB i s =
  let rec loop s =
    let* () = IO.printl (Printf.sprintf "tB(%d)" i) in
    let* s = send s#role_A#msg () in
    let* (`msg (_, s)) = receive s#role_C in
    let* () = Unix.sleepf 0.1 in
    loop s
  in
  loop s

let rec tC s =
  let* () = IO.printl "tC" in
  let* (`msg ((), s)) = receive s#role_A in
  let* s = send_many s#role_B#msg (fun _ -> ()) in
  tC s

let () =
  let g = gen_mult [ 1; 10; 1 ] (prot ()) in
  let threads =
    [ Thread.create (tA @@ get_ch a g) 0; Thread.create tC (get_ch c g) ]
    @ List.mapi (fun i s -> Thread.create (tB i) s) (get_ch_list b g)
  in
  IO.main_run (IO_list.iter Thread.join threads)
