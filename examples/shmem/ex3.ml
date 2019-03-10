(* simple loop *)
open Mpst_shmem.Session
open Mpst_shmem.Global
open Mpst_shmem.ThreeParty

let (>>=) = Lwt.(>>=)

let mk_g () =
  let rec g =
    lazy
      begin
        (b --> a) msg @@
        (a --> c) msg @@
        (c --> b) msg @@
        loop g
      end
  in
  Lazy.force g

let rec tA s =
  Lwt_io.print "tA\n" >>= fun () ->
  receive `B s >>= fun (`msg((),s)) ->
  let s = send `C (fun x->x#msg) () s in
  tA s

let rec tB s =
  Lwt_io.print "tB\n" >>= fun () ->
  let s = send `A (fun x->x#msg) () s in
  receive `C s >>= fun (`msg((),s)) ->
  tB s

let rec tC s =
  Lwt_io.print "tC\n" >>= fun () ->
  receive `A s >>= fun (`msg((),s)) ->
  let s = send `B (fun x->x#msg) () s in
  tC s

let () =
  let g = mk_g () in
  Lwt_main.run (Lwt.join [tA (get_sess a g); tB (get_sess b g); tC (get_sess c g)])
