open Mpst.Session
open Mpst.Session.Local
open Mpst.Session.MPST

(* A global protocol between A, B, and C *)
let g =
    (c --> a) (msg int) @@
    (a -%%-> b)
      ~left:((a,b),
             (b --> c) right @@
             (b --> a) (msg str) @@
             finish)
      ~right:((a,b),
             (b --> a) (msg int) @@
             (b --> c) left @@
             (c --> a) (msg str) @@
             finish)

let pa = get_sess a g
let pb = get_sess b g
let pc = get_sess c g

(* participant A *)
let (t1 : unit Lwt.t) =
  let s = pa in
  let open Lwt in
  receive C s >>= fun (x, s) -> begin
      if x = 0 then begin
          let s = select_left_ B s in
          receive B s >>= fun (str,s) ->
          Printf.printf "A: B says: %s\n" str;
          close s;
          return ()
        end else begin
          let s = select_right_ B s in
          receive B s >>= fun (x,s) ->
          receive C s >>= fun (str,s) ->
          Printf.printf "A: B says: %d, C says: %s\n" x str;
          close s;
          return ()
        end;
    end >>= fun () ->
  print_endline "A finished.";
  return ()

(* participant B *)
let (t2 : unit Lwt.t) =
  let s = pb in
  let open Lwt in
  branch A s >>= begin
      function
      | Left s ->
         let s = select_right C s in
         let s = send A "Hooray!" s in
         close s;
         return ()
      | Right s ->
         let s = send A 1234 s in
         let s = select_left C s in
         close s;
         return ()
    end >>= fun () ->
  print_endline "B finished.";
  return ()

(* participant C *)
let (t3 : unit Lwt.t) =
  let s = pc in
  let open Lwt in
  print_endline "C: enter a number:";
  Lwt_io.read_line Lwt_io.stdin >>= fun line ->
  let num = int_of_string line in
  let s = send A num s in
  branch B s >>= begin
      function
      | Left s -> begin
          let s = send A "Hello, A!" s in
          close s;
          return ()
        end
      | Right s -> begin
          close s;
          return ()
        end
    end >>= fun () ->
  print_endline "C finished.";
  return ()


let () =
  Lwt_main.run (Lwt.join [t1; t2; t3])
