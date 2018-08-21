open Mpst.Session3.MPST
open Lwt

let rec mk_g () =
  (b --> a) msg @@
  (a --> c) msg @@
  (c --> b) msg @@
  loop mk_g

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

let () =
  let g = mk_g () in
  Lwt_main.run (Lwt.join [t1 (get_sess a g); t2 (get_sess b g); t3 (get_sess c g)])

