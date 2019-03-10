open Implicit2.Util
open Session
open Global

let a = {role=`A; lens=Fst}
let b = {role=`B; lens=Next Fst}
let c = {role=`C; lens=Next (Next Fst)}
let lv = Lazy.from_val

let finish = lv (Cons(lv Close,
             lv @@ Cons(lv Close,
             lv @@ Cons(lv Close, lv Nil))))

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

let pa, pb, pc =
  let g = create_g () in
  get_sess a g, get_sess b g, get_sess c g

(* participant A *)
let t1 b_conn c_conn : unit Lwt.t =
  let b : [`B] one ep = EPOne(b_conn,`B)
  and c : [`C] one ep = EPOne(c_conn,`C)
  in
  let s = pa in
  let open Lwt in
  receive c s >>= fun (`msg(x, s)) -> begin
      if x = 0 then begin
          let s = send b (fun x->x#left) () s in
          receive b s >>= fun (`msg(str,s)) ->
          Printf.printf "A) B says: %s\n" str;
          close s;
          return ()
        end else begin
          print_endline "t1 right0";
          let s = send b (fun x->x#right) () s in
          print_endline "t1 right01";
          receive b s >>= fun (`msg(x,s)) ->
          print_endline "t1 right1";
          receive c s >>= fun (`msg(str,s)) ->
          print_endline "t1 right2";
          Printf.printf "A) B says: %d, C says: %s\n" x str;
          close s;
          return ()
        end;
    end >>= fun () ->
  print_endline "A finished.";
  return ()

(* participant B *)
let t2 a_conn c_conn : unit Lwt.t =
  let a : [`A] one ep = EPOne(a_conn,`A)
  and c : [`C] one ep = EPOne(c_conn,`C)
  in
  let s = pb in
  receive a s >>= begin
      function
      | `left(_,s) ->
         print_endline "t2 left ";
         let s = send c (fun x->x#right) () s in
         let s = send a (fun x->x#msg) "Hooray!" s in
         close s;
         Lwt.return ()
      | `right(_,s) ->
         print_endline "t2 right ";
         let s = send a (fun x->x#msg) 1234 s in
         let s = send c (fun x->x#left) () s in
         close s;
         Lwt.return ()
    end >>= fun () ->
  print_endline "B finished.";
  Lwt.return ()

(* participant C *)
let t3 a_conn b_conn : unit Lwt.t =
  let a : [`A] one ep = EPOne(a_conn,`A)
  and b : [`B] one ep = EPOne(b_conn,`B)
  in
  let s = pc in
  let open Lwt in
  print_endline "C: enter a number (positive or zero or negative):";
  Lwt_io.read_line Lwt_io.stdin >>= fun line ->
  let num = int_of_string line in
  let s = send a (fun x->x#msg) num s in
  receive b s >>= begin
      function
      | `left(_,s) -> begin
          let s = send a (fun x->x#msg) "Hello, A!" s in
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
  let [b_conn; c_conn] =
    forkmany [
        {procname="B";procbody=(fun [a_conn;c_conn] -> Lwt_main.run (t2 a_conn c_conn))};
        {procname="C";procbody=(fun [a_conn;b_conn] -> Lwt_main.run (t3 a_conn b_conn))}] in
  Lwt_main.run (t1 b_conn c_conn)

