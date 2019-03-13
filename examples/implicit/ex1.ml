open Mpst_implicit.Session
open Mpst_implicit.Global
open Mpst_implicit.Util

let a = {role=`A; lens=Fst}
let b = {role=`B; lens=Next Fst}
let c = {role=`C; lens=Next (Next Fst)}
let lv = Lazy.from_val

let finish = one @@ one @@ one @@ nil

let (>>=) = Lwt.(>>=)
          
(* A global protocol between A, B, and C *)
let create_g () =
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

(* participant A *)
let t1 s : unit Lwt.t =
  let open Lwt in
  receive `C s >>= fun (`msg(x, s)) -> begin
      if x = 0 then begin
          let s = send `B (fun x->x#left) () s in
          receive `B s >>= fun (`msg(str,s)) ->
          Printf.printf "A) B says: %s\n" str;
          close s;
          return ()
        end else begin
          print_endline "t1 right0";
          let s = send `B (fun x->x#right) () s in
          print_endline "t1 right01";
          receive `B s >>= fun (`msg(x,s)) ->
          print_endline "t1 right1";
          receive `C s >>= fun (`msg(str,s)) ->
          print_endline "t1 right2";
          Printf.printf "A) B says: %d, C says: %s\n" x str;
          close s;
          return ()
        end;
    end >>= fun () ->
  print_endline "A finished.";
  return ()

(* participant B *)
let t2 s : unit Lwt.t =
  receive `A s >>= begin
      function
      | `left(_,s) ->
         print_endline "t2 left ";
         let s = send `C (fun x->x#right) () s in
         let s = send `A (fun x->x#msg) "Hooray!" s in
         close s;
         Lwt.return ()
      | `right(_,s) ->
         print_endline "t2 right ";
         let s = send `A (fun x->x#msg) 1234 s in
         let s = send `C (fun x->x#left) () s in
         close s;
         Lwt.return ()
    end >>= fun () ->
  print_endline "B finished.";
  Lwt.return ()

(* participant C *)
let t3 s : unit Lwt.t =
  let open Lwt in
  print_endline "C: enter a number (positive or zero or negative):";
  Lwt_io.read_line Lwt_io.stdin >>= fun line ->
  let num = int_of_string line in
  let s = send `A (fun x->x#msg) num s in
  receive `B s >>= begin
      function
      | `left(_,s) -> begin
          let s = send `B (fun x->x#msg) "Hello, A!" s in
          close s;
          return ()
        end
      | `right(_,s) -> begin
          print_endline "t3 right ";
          close s;
          return ()
        end
    end >>= fun () ->
  print_endline "C finished.";
  return ()
  
let () =
  let g = create_g () in
  let g = mkpipes [`A;`B;`C] g in
  let pa, pb, pc = get_sess a g, get_sess b g, get_sess c g in
  fork (fun () -> Lwt_main.run (t2 pb));
  fork (fun () -> Lwt_main.run (t3 pc));
  Lwt_main.run (t1 pa)

