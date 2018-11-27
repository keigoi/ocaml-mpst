(* delegation *)
open Mpst.ThreeParty
let (>>=) = Lwt.(>>=)

module M = Marshal_example

let g_bc' (m:('k1 dist,'k2 dist) standard) =
  (b --> c) (left m) @@
  finish

let g_bc (m:('k1 dist,'k2 dist) standard) =
  (c --> b) (left m) @@
  g_bc' m
  
  
(* A global protocol between A, B, and C *)
let g_abc (m:('k1 dist,'k2 dist) standard) =
  let ctob = b.lens.get (g_bc' m) in
  (a --> b) (msg m) @@
  (b --> a) (deleg_dist ctob m#ch_deleg) @@
  (c --> a) (msg m) @@
  finish
  
let cab = M.create_shmem_channel ()
let cac = M.create_shmem_channel ()
let cbc = M.create_shmem_channel ()

let ta g_abc =
  M.shmem_accept cab >>= fun kab ->
  M.shmem_accept cac >>= fun kac ->
  print_endline "ta connected";
  let s = get_sess__ a ((), Conn kab, Conn kac) g_abc in

  let s = send b (fun x->x#msg) () s in
  receive b s >>= fun (`deleg(s_bc,s)) ->
  print_endline "ta 1";
  receive c s >>= fun (`msg((),s)) ->
  print_endline "ta 2";
  close s;
  let s_bc = send c (fun x->x#left) () s_bc in
  close s_bc;
  print_endline "ta finished";
  Lwt.return_unit
                       
  
let tb g_abc g_bc =
  let kab = M.shmem_connect cab in
  M.shmem_accept cbc >>= fun kbc ->
  print_endline "tb connected";
  let s = get_sess__ b (Conn kab, (), ()) g_abc in
  let s_bc = get_sess__ b ((), (), Conn kbc) g_bc in

  receive c s_bc >>= fun (`left((),s_bc)) ->
  print_endline "tb 1";
  receive a s >>= fun (`msg((),s)) ->
  print_endline "tb 2";
  let s = send a (fun x->x#deleg) s_bc s in
  close s;
  print_endline "tb finished";
  Lwt.return_unit
      
let tc g_abc g_bc =
  let kac = M.shmem_connect cac in
  let kbc = M.shmem_connect cbc in
  print_endline "tc connected";
  let s = get_sess__ c (Conn kac, (), ()) g_abc in
  let s_bc = get_sess__ c ((), Conn kbc, ()) g_bc in

  let s_bc = send b (fun x->x#left) () s_bc in
  let s = send a (fun x->x#msg) () s in
  close s;
  receive b s_bc >>= fun (`left((),s_bc)) ->
  close s_bc;
  print_endline "tc finished";
  Lwt.return_unit

let main () =
  let g_abc = g_abc (new M.marshal) in
  let g_bc = g_bc (new M.marshal) in

  Lwt_main.run @@
    Lwt.join [ta g_abc;
              tb g_abc g_bc;
              tc g_abc g_bc];
    ()

let () =
  main ()
