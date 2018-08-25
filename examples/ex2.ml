(*  ( *--> ) operator *)

open Mpst.ThreeParty
open Mpst.ThreeParty.Shmem
let (>>=) = Lwt.(>>=)

let create_g () =
  (a *--> b) (msg ()) @@
  (b *--> c) (right ()) @@
  (c --> a) (msg ()) @@
  (c --> b) (msg ()) @@
  finish

let () =
  let g = create_g () in
  let ta =
    let s = get_sess a g in
    send_receive (B, C) (fun x->x#msg) () s >>= fun (`msg((),s)) ->
    close s;
    print_endline "t1 done";
    Lwt.return ()
  in
  let tb =
    let s = get_sess b g in
    receive A s >>= fun (`msg((),s)) ->
    send_receive (C, C) (fun x->x#right) () s >>= fun (`msg((),s)) ->
    close s;
    print_endline "t2 done";
    Lwt.return ()
  in
  let tc =
    let s = get_sess c g in
    receive B s >>= fun (`right((),s)) ->
    let s = send A (fun x->x#msg) () s in
    let s = send B (fun x->x#msg) () s in
    close s;
    print_endline "t3 done";
    Lwt.return ()
  in
  Lwt_main.run @@ Lwt.join [ta; tb; tc]
