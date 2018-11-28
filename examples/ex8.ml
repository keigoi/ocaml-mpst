(* delegation *)
open Mpst.ThreeParty
let (>>=) = Lwt.(>>=)

module M = Marshal_example

let g_bc' (m:('k1 dist,'k2 dist) standard) =
  (b --> c) (left m) @@
  discon (b,b) (c,c) @@
  finish_

let g_bc (m:('k1 dist,'k2 dist) standard) =
  (c --> b) (left m) @@
  g_bc' m

(* A global protocol between A, B, and C *)
let g_abc (m:('k1 dist,'k2 dist) standard) =
  let ctob = b.lens.get (g_bc' m) in
  (a --> b) (msg m) @@
  (c --> a) (msg m) @@
  (b --> a) (deleg_dist ctob m#ch_deleg) @@
  discon (a,a) (b,b) @@
  discon (a,a) (c,c) @@      
  finish_
  
let cab = M.create_shmem_channel ()
let cac = M.create_shmem_channel ()
let cbc = M.create_shmem_channel ()

let ta () =
  let g_abc = g_abc (new M.marshal) in
  M.shmem_accept "a" cab >>= fun kab ->
  M.shmem_accept "a" cac >>= fun kac ->
  let s = get_sess__ a ((), Conn kab, Conn kac) g_abc in

  let s = send b (fun x->x#msg) () s in
  receive c s >>= fun (`msg((),s)) ->
  receive b s >>= fun (`deleg(s_bc,s)) ->
  let s = disconnect b s in
  let s = disconnect c s in
  close s;
  let s_bc = send c (fun x->x#left) () s_bc in
  let s_bc = disconnect c s_bc in
  close s_bc;
  print_endline "ta finished";
  Lwt.return_unit
  
let tb () =
  let g_bc = g_bc (new M.marshal) in
  let g_abc = g_abc (new M.marshal) in
  let kab = M.shmem_connect "b" cab in
  M.shmem_accept "b" cbc >>= fun kbc ->
  let s = get_sess__ b (Conn kab, (), ()) g_abc in
  let s_bc = get_sess__ b ((), (), Conn kbc) g_bc in

  receive c s_bc >>= fun (`left((),s_bc)) ->
  receive a s >>= fun (`msg((),s)) ->
  let s = send a (fun x->x#deleg) s_bc s in
  let s = disconnect a s in
  close s;
  print_endline "tb finished";
  Lwt.return_unit
      
let tc () =
  let g_bc = g_bc (new M.marshal) in
  let g_abc = g_abc (new M.marshal) in
  let kac = M.shmem_connect "c" cac in
  let kbc = M.shmem_connect "c" cbc in
  let s = get_sess__ c (Conn kac, (), ()) g_abc in
  let s_bc = get_sess__ c ((), Conn kbc, ()) g_bc in

  let s_bc = send b (fun x->x#left) () s_bc in
  let s = send a (fun x->x#msg) () s in
  let s = disconnect a s in
  close s;
  receive b s_bc >>= fun (`left((),s_bc)) ->
  let s_bc = disconnect b s_bc in
  close s_bc;
  print_endline "tc finished";
  Lwt.return_unit

let main () =
  Lwt_main.run @@
    Lwt.join [ta ();
              tb ();
              tc ()];
    ()

let () =
  main ()
