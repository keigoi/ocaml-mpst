(* multicasts *)
open Concur_shims
open Mpst
open Mpst.Util

let (let*) = IO.bind

let finish = finish_with_multirole ~at:b

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
let t1 () =
  Thread.create (fun () ->
      let s = pa in
      let* `msg(x,s) =  receive s#role_C in
      let* () = begin
        Printf.printf "received: %d\n" x;
        if x = 0 then begin
          let* s = send_many s#role_B#left (fun _ -> ()) in
          print_endline "sent left";
          let* `msg(str,s) = receive_many s#role_B in
          print_endline "received";
          str |> List.iter (Printf.printf "A) B says: %s\n");
          close s
        end else begin
          let* s = send_many s#role_B#right (fun _ -> ()) in
          print_endline "sent right";
          let* `msg(xs,s) = receive_many s#role_B in
          print_endline "received1";
          let* `msg(str,s) = receive s#role_C in
          print_endline "received2";
          List.iteri (fun i x -> Printf.printf "A) B(%d) says: %d, C says: %s\n" i x str) xs;
          close s
        end
      end in
      IO.printl "A finished."
    ) ()

(* participant B *)
let t2 () =
  pb |> List.mapi (fun i ->
      Thread.create (fun s ->
          Printf.printf "B(%d): waiting.\n" i;
          let* var = receive s#role_A in
          let* () =
            begin match var with
              | `left(_,s) ->
                Printf.printf "B(%d): left.\n" i;
                let* s = send s#role_C#right () in
                let* s = send s#role_A#msg (Printf.sprintf "Hooray! %d" i) in
                close s
              | `right(_,s) ->
                Printf.printf "B(%d): right.\n" i;
                let* s = send s#role_A#msg (1234 * (i+1)) in
                let* s = send s#role_C#left () in
                close s
            end
          in
          Printf.printf "B(%d) finished.\n" i;
          IO.return ()
        ))



(* participant C *)
let t3 () =
  Thread.create (fun () ->
      let s = pc in
      let open Lwt in
      print_endline "C: enter a number (positive or zero or negative):";
      let* line = IO.read_line IO.stdin in
      let num = int_of_string line in
      let* s = send s#role_A#msg num in
      let* var = receive_many s#role_B in
      let* () =
        begin match var with
          | `left(_,s) -> begin
              send s#role_A#msg "Hello, A!" >>= fun s ->
              close s
            end
          | `right(_,s) -> begin
              close s
            end
        end in
      IO.printl "C finished."
    ) ()

let () =
  IO.main_run (IO_list.iter Thread.join ([t1 (); t3 ()] @ t2 ()))
