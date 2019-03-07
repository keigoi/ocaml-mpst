open Shmem.Session
open Shmem.Global

let a = {role=`A; lens=FstProt}
let b = {role=`B; lens=Next FstList}
let c = {role=`C; lens=Next (Next FstProt)}
let lv = Lazy.from_val
      
let finish = lv (ConsProt(lv Close,ConsList(lv [Close;Close],ConsProt(lv Close, Nil))))
      
let msg = {select_label=(fun f -> object method msg v=f v end);
           offer_label=(fun (v,c) -> `msg(v,c))}
let left = {select_label=(fun f -> object method left v=f v end);
           offer_label=(fun (v,c) -> `left(v,c))}
let right = {select_label=(fun f -> object method right v=f v end);
           offer_label=(fun (v,c) -> `right(v,c))}

let left_or_right =
  {label_merge=(fun ol or_ -> object method left=ol#left method right=or_#right end)}

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

let lazy pa, lazy pb, lazy pc =
  let g = create_g () in
  lens_get a.lens g, lens_get b.lens g, lens_get c.lens g

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
          let s = send b (fun x->x#left) [(); ()] s in
          receive b s >>= fun (`msg(str,s)) ->
          str |> List.iter (Printf.printf "A) B says: %s\n");
          close s;
          return ()
        end else begin
          let s = send b (fun x->x#right) [(); ()] s in
          receive b s >>= fun (`msg(xs,s)) ->
          receive c s >>= fun (`msg(str,s)) ->
          List.iter (fun x -> Printf.printf "A) B says: %d, C says: %s\n" x str) xs;
          close s;
          return ()
        end;
    end >>= fun () ->
  print_endline "A finished.";
  return ()

(* participant B *)
let t2 : unit Lwt.t list =
  pb |>
  List.map begin fun s ->
  receive a s >>= begin
      function
      | `left(_,s) ->
         print_endline "B: left.";
         let s = send c (fun x->x#right) () s in
         let s = send a (fun x->x#msg) "Hooray!" s in
         close s;
         Lwt.return ()
      | `right(_,s) ->
         print_endline "B: right.";
         let s = send a (fun x->x#msg) 1234 s in
         let s = send c (fun x->x#left) () s in
         close s;
         Lwt.return ()
    end >>= fun () ->
  print_endline "B finished.";
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
