(* delegation *)
open Mpst.ThreeParty
let (>>=) = Lwt.(>>=)

let g_bc' (m:('k1,'k2) #standard) =
  (b --> c) (left m) @@
  finish_

let g_bc (m:('k1,'k2) #standard) =
  (c --> b) (left m) @@
  g_bc' m
  
  
(* A global protocol between A, B, and C *)
let g_abc (m:('k1,'k2) standard_deleg) =
  let ctob = (Lazy.force (g_bc' m))#b in
  (a --> b) (msg m) @@
  (b --> a) (deleg ctob m) @@
  (c --> a) (msg m) @@
  finish_

let ta s =
  let s = send b (fun x->x#msg) () s in
  receive b s >>= fun (`deleg(s_bc,s)) ->
  receive c s >>= fun (`msg((),s)) ->
  close s;
  let s_bc = send c (fun x->x#left) () s_bc in
  close s_bc;
  print_endline "ta finished";
  Lwt.return_unit
                       
  
let tb s s_bc =
  receive c s_bc >>= fun (`left((),s_bc)) ->
  receive a s >>= fun (`msg((),s)) ->
  let s = send a (fun x->x#deleg) s_bc s in
  close s;
  print_endline "tb finished";
  Lwt.return_unit
      
let tc s s_bc =
  let s_bc = send b (fun x->x#left) () s_bc in
  let s = send a (fun x->x#msg) () s in
  close s;
  receive b s_bc >>= fun (`left((),s_bc)) ->
  close s_bc;
  Lwt.return_unit

let main () =
  let g_abc = g_abc (new Marshal_example.marshal) in
  let g_bc = g_bc (new Marshal_example.marshal) in
  Lwt_main.run @@
    Lwt.join [ta (get_sess a g_abc);
              tb (get_sess b g_abc) (get_sess b g_bc);
              tc (get_sess c g_abc) (get_sess c g_bc)];
    ()

let () = main ()
