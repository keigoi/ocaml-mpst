(* multicasts *)
open Mpst_lwt

let (>>=) = Lwt.(>>=)

(* A global protocol between A, B, and C *)
let prot =
    (c --> a) msg @@
    choice_at a (to_b left_or_right)
      (a, (scatter a b) left @@
          (gather b c) right @@
          (gather b a) msg @@
          finish)
      (a, (scatter a b) right @@
          (gather b a) msg @@
          (gather b c) left @@
          (c --> a) msg @@
          finish)

let pa, pb, pc =
  let g = gen_mult [1;10;1] prot in
  get_ch a g, get_ch_list b g, get_ch c g

(* participant A *)
let t1 : unit Lwt.t =
  let s = pa in
  let open Lwt in
  receive s#role_C >>= fun (`msg(x, s)) -> begin
      Printf.printf "received: %d\n" x;
      if x = 0 then begin
          sendmany s#role_B#left (fun _ -> ()) >>= fun s ->
          print_endline "sent left";
          receive s#role_B >>= fun (`msg(str,s)) ->
          print_endline "received";
          str |> List.iter (Printf.printf "A) B says: %s\n");
          close s;
          return ()
        end else begin
          sendmany s#role_B#right (fun _ -> ()) >>= fun s ->
          print_endline "sent right";
          receive s#role_B >>= fun (`msg(xs,s)) ->
          print_endline "received1";
          receive s#role_C >>= fun (`msg(str,s)) ->
          print_endline "received2";
          List.iteri (fun i x -> Printf.printf "A) B(%d) says: %d, C says: %s\n" i x str) xs;
          close s;
          return ()
        end;
    end >>= fun () ->
  print_endline "A finished.";
  return ()

(* participant B *)
let t2 : unit Lwt.t list =
  pb |>
  List.mapi begin fun i s ->
  Printf.printf "B(%d): waiting.\n" i;
  receive s#role_A >>= begin
      function
      | `left(_,s) ->
         Printf.printf "B(%d): left.\n" i;
         send s#role_C#right () >>= fun s ->
         send s#role_A#msg (Printf.sprintf "Hooray! %d" i) >>= fun s ->
         close s;
         Lwt.return ()
      | `right(_,s) ->
         Printf.printf "B(%d): right.\n" i;
         send s#role_A#msg (1234 * (i+1)) >>= fun s ->
         send s#role_C#left () >>= fun s ->
         close s;
         Lwt.return ()
    end >>= fun () ->
  Printf.printf "B(%d) finished.\n" i;
  Lwt.return ()
  end


(* participant C *)
let t3 : unit Lwt.t =
  let s = pc in
  let open Lwt in
  print_endline "C: enter a number (positive or zero or negative):";
  Lwt_io.read_line Lwt_io.stdin >>= fun line ->
  let num = int_of_string line in
  send s#role_A#msg num >>= fun s ->
  receive s#role_B >>= begin
      function
      | `left(_,s) -> begin
          send s#role_A#msg "Hello, A!" >>= fun s ->
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
  Lwt_main.run (Lwt.join [t1; Lwt.join t2; t3])
