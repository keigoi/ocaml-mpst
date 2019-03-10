open Mpst_shmem.Session
open Mpst_shmem.Global
open Mpst_shmem.ThreeParty
      
let finish = one @@ many 10 @@ one @@ nil

let (>>=) = Lwt.(>>=)
          
(* A global protocol between A, B, and C *)
let create_g () =
    (c --> a) msg @@
    choice_at a left_or_right
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

let a : [`A] = `A
let b : [`B] = `B
let c : [`C] = `C
  
(* participant A *)
let t1 : unit Lwt.t =
  let s = pa in
  let open Lwt in
  receive c s >>= fun (`msg(x, s)) -> begin
      Printf.printf "received: %d\n" x;
      if x = 0 then begin
          let s = send b (fun x->x#left) (fun _ -> ()) s in
          receive b s >>= fun (`msg(str,s)) ->
          str |> List.iter (Printf.printf "A) B says: %s\n");
          close s;
          return ()
        end else begin
          let s = send b (fun x->x#right) (fun _ -> ()) s in
          receive b s >>= fun (`msg(xs,s)) ->
          receive c s >>= fun (`msg(str,s)) ->
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
  end


(* participant C *)
let t3 : unit Lwt.t =
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
          close s;
          return ()
        end
    end >>= fun () ->
  print_endline "C finished.";
  return ()

let () =
  Lwt_main.run (Lwt.join [t1; Lwt.join t2; t3])
