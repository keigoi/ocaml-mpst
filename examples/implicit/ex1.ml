(* open Mpst_implicit
 * open Mpst_implicit.IPC
 * 
 * let lv = Lazy.from_val
 * 
 * let (>>=) = Lwt.(>>=)
 *           
 * (\* A global protocol between A, B, and C *\)
 * let create_g () =
 *     (c --> a) msg @@
 *     choice_at a (to_b left_or_right)
 *       (a, (a --> b) left @@
 *           (b --> c) right @@
 *           (b --> a) msg @@
 *           finish)
 *       (a, (a --> b) right @@
 *           (b --> a) msg @@
 *           (b --> c) left @@
 *           (c --> a) msg @@
 *           finish)
 * 
 * (\* participant A *\)
 * let t1 s : unit Lwt.t =
 *   let open Lwt in
 *   s#role_C >>= fun (`msg(x, s)) -> begin
 *       if x = 0 then begin
 *           let s = s#role_B#left () in
 *           s#role_B >>= fun (`msg(str,s)) ->
 *           Printf.printf "A) B says: %s\n" str;
 *           close s;
 *           return ()
 *         end else begin
 *           print_endline "t1 right0";
 *           let s = s#role_B#right in
 *           print_endline "t1 right01";
 *           s#role_B >>= fun (`msg(x,s)) ->
 *           print_endline "t1 right1";
 *           s#role_C >>= fun (`msg(str,s)) ->
 *           print_endline "t1 right2";
 *           Printf.printf "A) B says: %d, C says: %s\n" x str;
 *           close s;
 *           return ()
 *         end;
 *     end >>= fun () ->
 *   print_endline "A finished.";
 *   return ()
 * 
 * (\* participant B *\)
 * let t2 s : unit Lwt.t =
 *   s#role_A >>= begin
 *       function
 *       | `left(_,s) ->
 *          print_endline "t2 left ";
 *          let s = s#role_C#right () in
 *          let s = s#role_A#msg "Hooray!" in
 *          close s;
 *          Lwt.return ()
 *       | `right(_,s) ->
 *          print_endline "t2 right ";
 *          let s = s#role_A#msg 1234 in
 *          let s = s#role_C#left () in
 *          close s;
 *          Lwt.return ()
 *     end >>= fun () ->
 *   print_endline "B finished.";
 *   Lwt.return ()
 * 
 * (\* participant C *\)
 * let t3 s : unit Lwt.t =
 *   let open Lwt in
 *   Random.self_init ();
 *   print_endline "C: enter a number (positive or zero or negative):";
 *   (\* Lwt_io.read_line Lwt_io.stdin >>= fun line ->
 *    * let num = int_of_string line in *\)
 *   let num =
 *     if Random.bool () then 0 else 1
 *   in
 *   let s = s#role_A#msg num in
 *   s#role_B >>= begin
 *       function
 *       | `left(_,s) -> begin
 *           let s = s#role_B#msg "Hello, A!" in
 *           close s;
 *           return ()
 *         end
 *       | `right(_,s) -> begin
 *           print_endline "t3 right ";
 *           close s;
 *           return ()
 *         end
 *     end >>= fun () ->
 *   print_endline "C finished.";
 *   return () *)

(* open Mpst_base.Connection
 * let () =
 *   let g = create_g () in
 *   let g = pipes [`A;`B;`C] g in
 *   let pa, pb, pc = get_sess a g, get_sess b g, get_sess c g in
 *   Mpst_base.fork (fun () -> Lwt_main.run (t2 pb));
 *   Mpst_base.fork (fun () -> Lwt_main.run (t3 pc));
 *   Lwt_main.run (t1 pa) *)

