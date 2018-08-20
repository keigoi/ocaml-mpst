open Mpst.Session3.MPST
open Lwt

let rec mk_g () =
  (b --> a) msg @@
  (a --> c) msg @@
  (c --> b) msg @@
  loop3 mk_g

let rec t1 s =
  print_endline "t1";
  receive (fun (B,x) -> x) s >>= fun (`msg((),s)) ->
  let s = send (fun (C,x) -> x) (fun x->x#msg) () s in
  t1 s

let rec t2 s =
  print_endline "t2";
  let s = send (fun (A,x) -> x) (fun x->x#msg) () s in
  receive (fun (C,x) -> x) s >>= fun (`msg((),s)) ->
  t2 s

let rec t3 s =
  print_endline "t3";
  receive (fun (A,x) -> x) s >>= fun (`msg((),s)) ->
  let s = send (fun (B,x) -> x) (fun x->x#msg) () s in
  t3 s

let g = mk_g ()
let () = print_endline "channel generated"
let _ = (fst a).get g
let () = print_endline "a"

let () =
  Lwt_main.run (Lwt.join [t1 ((fst a).get g); t2 ((fst b).get g); t3 ((fst c).get g)])

