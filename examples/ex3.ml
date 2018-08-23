(* simple loop *)
open Mpst.ThreeParty
open Mpst.ThreeParty.Shmem
let (>>=) = Lwt.(>>=)

let mk_g () =
  let rec g =
    lazy
      begin
        (b --> a) (msg ()) @@
        (a --> c) (msg ()) @@
        (c --> b) (msg ()) @@
        loop_ g
      end
  in
  Lazy.force g

let rec tA s =
  print_endline "tA";
  receive B s >>= fun (`msg((),s)) ->
  let s = send C (fun x->x#msg) () s in
  tA s

let rec tB s =
  print_endline "tB";
  let s = send A (fun x->x#msg) () s in
  receive C s >>= fun (`msg((),s)) ->
  tB s

let rec tC s =
  print_endline "tC";
  receive A s >>= fun (`msg((),s)) ->
  let s = send B (fun x->x#msg) () s in
  tC s

let () =
  let g = mk_g () in
  Lwt_main.run (Lwt.join [tA (get_sess a g); tB (get_sess b g); tC (get_sess c g)])
