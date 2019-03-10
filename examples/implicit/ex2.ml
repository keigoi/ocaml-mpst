open Mpst_implicit.Util
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
    choicemany_at a left_or_right
      (a, (a -->> b) left @@
          (b >>-- c) right @@
          (b >>-- a) msg @@
          finish)
      (a, (a -->> b) right @@
          (b >>-- a) msg @@
          (b >>-- c) left @@
          (c --> a) msg @@
          finish)

let pa, pb, pc =
  let g = create_g () in
  get_sess a g, get_sess_many b g, get_sess c g

(* participant A *)
let t1 b_conns c_conn : unit Lwt.t =
  let b : [`B] many ep = EPMany(b_conns,`B)
  and c : [`C] one ep = EPOne(c_conn,`C)
  in
  let s = pa in
  let open Lwt in
  receive c s >>= fun (`msg(x, s)) -> begin
      Printf.printf "received: %d\n" x;
      if x = 0 then begin
          let s = multicast b (fun x->x#left) (fun _ -> ()) s in
          gather b s >>= fun (`msg(str,s)) ->
          str |> List.iter (Printf.printf "A) B says: %s\n");
          close s;
          return ()
        end else begin
          let s = multicast b (fun x->x#right) (fun _ -> ()) s in
          gather b s >>= fun (`msg(xs,s)) ->
          List.iteri (fun i x -> Printf.printf "A) B(%d) says: %d\n" i x) xs;
          receive c s >>= fun (`msg(str,s)) ->
          Printf.printf "C says: %s\n" str;
          close s;
          return ()
        end;
    end >>= fun () ->
  print_endline "A finished.";
  return ()

(* participant B *)
let t2 i a_conn c_conn : unit Lwt.t =
  let a : [`A] one ep = EPOne(a_conn,`A)
  and c : [`C] one ep = EPOne(c_conn,`C)
  in
  let s = pb in
  receive a s >>= begin
      function
      | `left(_,s) ->
         Printf.printf "B(%d): left.\n" i;
         let s = send c (fun x->x#right) () s in
         let s = send a (fun x->x#msg) (Printf.sprintf "Hooray! %d" i) s in
         close s;
         Lwt.return ()
      | `right(_,s) ->
         Printf.printf "B(%d): right.\n" i;
         let s = send a (fun x->x#msg) (1234 * (i+1)) s in
         let s = send c (fun x->x#left) () s in
         close s;
         Lwt.return ()
    end >>= fun () ->
  Printf.printf "B(%d) finished.\n" i;
  Lwt.return ()


(* participant C *)
let t3 a_conn b_conns : unit Lwt.t =
  let a : [`A] one ep = EPOne(a_conn,`A)
  and b : [`B] many ep = EPMany(b_conns,`B)
  in
  let s = pc in
  let open Lwt in
  print_endline "C: enter a number (positive or zero or negative):";
  Lwt_io.read_line Lwt_io.stdin >>= fun line ->
  let num = int_of_string line in
  let s = send a (fun x->x#msg) num s in
  gather b s >>= begin
      function
      | `left(_,s) -> begin
          let s = send a (fun x->x#msg) "Hello, A!" s in
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

(* let () =
 *   let xs =
 *     forkmany_groups [
 *         {groupname="B";count=10;groupbody=(fun i xs -> Printf.printf "B:%d\n" (List.length xs))};
 *         {groupname="C";count=1;groupbody=(fun _ xs -> Printf.printf "C:%d\n" (List.length xs))}
 *       ]
 *   in
 *   Printf.printf "A:%d\n" @@ List.length xs *)

let () =
  let [b_conns; [c_conn]] =
    forkmany_groups [
        {groupname="B";count=5;groupbody=(fun i [[a_conn];[c_conn]] -> Lwt_main.run (t2 i a_conn c_conn))};
        {groupname="C";count=1;groupbody=(fun _ [[a_conn];b_conns] -> Lwt_main.run (t3 a_conn b_conns))}] in
  Lwt_main.run (t1 b_conns c_conn)
