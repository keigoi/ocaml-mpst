(* delegation *)
open Mpst_explicit.Session
open Mpst_explicit.Global
open Mpst_explicit.Parties
open Mpst_explicit.Util.Labels
let (>>=) = Lwt.(>>=)

module M = Marshal_example

let finish_ =
  lv@@Cons(lv@@Prot Close,lv@@Cons(lv@@Prot Close,lv@@Cons(lv@@Prot Close,lv Nil)))

let emp = Cons(lv Unit,lv@@Cons(lv Unit,lv@@Cons(lv Unit,lv Nil)))

let get_sess_ r c = Sess(emp, unprot @@ lens_get_ r.lens c)
         
let g_bc' (m:('k1,'k2) standard) =
  (b --> c) (left m) @@
  discon (b,b) (c,c) @@
  finish_

let g_bc (m:('k1,'k2) standard) =
  ((c,c) -!-> (b,b)) (left m) @@
  g_bc' m

(* A global protocol between A, B, and C *)
let g_abc (m:('k1,'k2) standard) =
  let ctob = unprot @@ lens_get_ b.lens (g_bc' m) in
  ((b,b) -!-> (a,a)) (msg m) @@
  ((c,c) -!-> (a,a)) (msg m) @@
  (b --> a) (deleg_dist ctob m#ch_deleg) @@
  discon (a,a) (b,b) @@
  discon (a,a) (c,c) @@      
  finish_
  
let cab = M.create_shmem_channel ()
let cac = M.create_shmem_channel ()

let ta () =
  let g_abc = g_abc (new M.marshal) in
  let s = get_sess_ a g_abc in

  M.shmem_accept "a" cab >>= fun kab ->
  accept b kab s >>= fun (`msg((),s)) ->
  M.shmem_accept "a" cac >>= fun kac ->
  accept c kac s >>= fun (`msg((),s)) ->
  receive b s >>= fun (`deleg(s_bc,s)) ->
  let s = disconnect b s in
  let s = disconnect c s in
  close s;
  let s_bc = send c (fun x->x#left) () s_bc in
  let s_bc = disconnect c s_bc in
  close s_bc;
  print_endline "ta finished";
  Lwt.return_unit
  
let cbc = M.create_shmem_channel ()

let tb () =
  let g_bc = g_bc (new M.marshal) in
  let g_abc = g_abc (new M.marshal) in
  let s = get_sess_ b g_abc in
  let s_bc = get_sess_ b g_bc in
  
  let kab = M.shmem_connect "b" cab in
  let s = request a (fun x->x#msg) () kab s in
  
  M.shmem_accept "b" cbc >>= fun kbc ->
  accept c kbc s_bc >>= fun (`left((),s_bc)) ->
  
  let s = send a (fun x->x#deleg) s_bc s in
  let s = disconnect a s in
  close s;
  print_endline "tb finished";
  Lwt.return_unit
      
let tc () =
  let g_bc = g_bc (new M.marshal) in
  let g_abc = g_abc (new M.marshal) in
  let s = get_sess_ c g_abc in
  let s_bc = get_sess_ c g_bc in
  let kac = M.shmem_connect "c" cac in
  let kbc = M.shmem_connect "c" cbc in

  let s_bc = request b (fun x->x#left) () kbc s_bc in
  let s = request a (fun x->x#msg) () kac s in
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
