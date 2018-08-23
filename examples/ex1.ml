open Mpst.ThreeParty
open Mpst.ThreeParty.Shmem
let (>>=) = Lwt.(>>=)

(* A global protocol between A, B, and C *)
let create_g () =
    (c --> a) (msg ()) @@
    (a -%%-> b) (leftright ())
      ~l1:((a,b),
             (b --> c) (right ()) @@
             (b --> a) (msg ()) @@
             finish)
      ~l2:((a,b),
             (b --> a) (msg ()) @@
             (b --> c) (left ()) @@
             (c --> a) (msg ()) @@
             finish)

let pa, pb, pc =
  let g = create_g () in
  get_sess a g, get_sess b g, get_sess c g

(* participant A *)
let (t1 : unit Lwt.t) =
  let s = pa in
  let open Lwt in
  receive C s >>= fun (`msg(x, s)) -> begin
      if x = 0 then begin
          let s = send B (fun x->x#left) () s in
          receive B s >>= fun (`msg(str,s)) ->
          Printf.printf "A) B says: %s\n" str;
          close s;
          return ()
        end else begin
          let s = send B (fun x->x#right) () s in
          receive B s >>= fun (`msg(x,s)) ->
          receive C s >>= fun (`msg(str,s)) ->
          Printf.printf "A) B says: %d, C says: %s\n" x str;
          close s;
          return ()
        end;
    end >>= fun () ->
  print_endline "A finished.";
  return ()

(* participant B *)
let (t2 : unit Lwt.t) =
  let s = pb in
  receive A s >>= begin
      function
      | `left(_,s) ->
         let s = send C (fun x->x#right) () s in
         let s = send A (fun x->x#msg) "Hooray!" s in
         close s;
         Lwt.return ()
      | `right(_,s) ->
         let s = send A (fun x->x#msg) 1234 s in
         let s = send C (fun x->x#left) () s in
         close s;
         Lwt.return ()
    end >>= fun () ->
  print_endline "B finished.";
  Lwt.return ()

(* participant C *)
let (t3 : unit Lwt.t) =
  let s = pc in
  let open Lwt in
  print_endline "C: enter a number (positive or zero or negative):";
  Lwt_io.read_line Lwt_io.stdin >>= fun line ->
  let num = int_of_string line in
  let s = send A (fun x->x#msg) num s in
  receive B s >>= begin
      function
      | `left(_,s) -> begin
          let s = send A (fun x->x#msg) "Hello, A!" s in
          close s;
          return ()
        end
      | `right(_,s) -> begin
          close s;
          return ()
        end
    end >>= fun () ->
  print_endline "C finished.";
  return ()

let () =
  Lwt_main.run (Lwt.join [t1; t2; t3])
