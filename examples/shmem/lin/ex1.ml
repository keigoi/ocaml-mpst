open Mpst_shmem.Lin
open Util
open Global
open Session
open LinMonad
open LinMonad.Op

let a : ([`A],_,_,_,_) role = {role=`A; lens=Fst}
let b : ([`B],_,_,_,_) role = {role=`B; lens=Next Fst}
let c : ([`C],_,_,_,_) role = {role=`C; lens=Next (Next Fst)}

let finish = one @@ one @@ one @@ nil

(* A global protocol between A, B, and C *)
let f () =
    (c --> a) msg @@
    choice_at a left_or_right
      (a, (a --> b) left @@
          (b --> c) right @@
          (b --> a) msg @@
          finish)
      (a, (a --> b) right @@
          (b --> a) msg @@
          (b --> c) left @@
          (c --> a) msg @@
          finish)

let g = create_global f [`A; `B; `C]

let _0 = Mpst_base.LinMonad.Fst

(* participant A *)
let t1 () =
  let%lin #_0 = connect g a in
  let%lin #_0 = unone _0 in
  let%lin `msg(x, #_0) = receive `C _0 in
  if x = 0 then begin
      let%lin #_0 = send `B (fun x->x#left) () _0 in
      let%lin `msg(str,#_0) = receive `B _0 in
      Printf.printf "A) B says: %s\n" str;
      close _0
    end else begin
      let%lin #_0 = send `B (fun x->x#right) () _0 in
      let%lin `msg(x, #_0) = receive `B _0 in
      let%lin `msg(str, #_0) = receive `C _0 in
      Printf.printf "A) B says: %d, C says: %s\n" x str;
      close _0
    end >>= fun () ->
  print_endline "A finished.";
  LinMonad.return ()

(* participant B *)
let t2 () =
  let%lin #_0 = connect g b in
  let%lin #_0 = unone _0 in
  begin match%lin receive `A _0 with
  | `left(_,#_0) ->
    let%lin #_0 = send `C (fun x->x#right) () _0 in
    let%lin #_0 = send `A (fun x->x#msg) "Hooray!" _0 in
    close _0
  | `right(_,#_0) ->
    let%lin #_0 = send `A (fun x->x#msg) 1234 _0 in
    let%lin #_0 = send `C (fun x->x#left) () _0 in
    close _0
  end >>= fun () ->
  print_endline "B finished.";
  return ()

(* participant C *)
let t3 () =
  let%lin #_0 = connect g c in
  let%lin #_0 = unone _0 in
  print_endline "C: enter a number (positive or zero or negative):";
  lift (Lwt_io.read_line Lwt_io.stdin) >>= fun line ->
  let num = int_of_string line in
  let%lin #_0 = send `A (fun x->x#msg) num _0 in
  begin match%lin receive `B _0 with
      | `left(_,#_0) -> begin
          let%lin #_0 = send `A (fun x->x#msg) "Hello, A!" _0 in
          close _0
        end
      | `right(_,#_0) -> begin
          close _0
        end
  end >>= fun () ->
  print_endline "C finished.";
  return ()

let run1 m = LinMonad.run (LinMonad.expand >>= fun () -> m >>= fun () -> LinMonad.shrink)
  
let m =
  Lwt_main.run @@ Lwt.join [run1 (t1 ()); run1 (t2 ()); run1 (t3 ())]
  
