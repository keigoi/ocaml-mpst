(* simple loop *)
open Mpst_shmem.Lin
open Util
open Global
open Session
open LinMonad
open LinMonad.Op

let a = {role=`A; lens=Fst}
let b = {role=`B; lens=Next Fst}
let c = {role=`C; lens=Next (Next Fst)}

let finish = one @@ one @@ one @@ nil

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

let _0 = LinMonad.Fst
  
let rec tA () =
  lift (Lwt_io.print "tA\n") >>= fun () ->
  let%lin `msg({data=()},#_0) = receive `B _0 in
  let%lin #_0 = send `C (fun x->x#msg) () _0 in
  tA ()

let rec tB () =
  lift (Lwt_io.print "tB\n") >>= fun () ->
  let%lin #_0 = send `A (fun x->x#msg) () _0 in
  let%lin `msg({data=()},#_0) = receive `C _0 in
  tB ()

let rec tC () =
  lift (Lwt_io.print "tC\n") >>= fun () ->
  let%lin `msg({data=()},#_0) = receive `A _0 in
  let%lin #_0 = send `B (fun x->x#msg) () _0 in
  tC ()

let run f g r =
  LinMonad.run begin
      LinMonad.expand >>= fun () ->
      let%lin #_0 = connect g r in
      let%lin #_0 = unone _0 in
      f () >>= fun () ->
      LinMonad.shrink
  end
  
let () =
  let g = create_global mk_g [`A; `B; `C] in
  Lwt_main.run (Lwt.join [run tA g a; run tB g b; run tC g c])
