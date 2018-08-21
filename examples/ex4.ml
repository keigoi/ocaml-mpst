open Mpst.Session3.MPST
open Lwt

let rec mk_g () =
  (b --> a) msg @@
    (a -%%-> b)
      ~left:((a,b),
             (b --> c) left @@
             (c --> a) msg @@
             finish)
      ~right:((a,b),
             (b --> c) right @@
             (c --> a) msg @@
             loop3 mk_g)

let rec t1 s : unit Lwt.t =
  let open Lwt in
  print_endline "t1";
  receive (fun (B,x) -> x) s >>= fun (`msg(v,s)) ->
  print_endline "t1 cont";
  if v > 0 then begin
      print_endline ">0";
      let s = send (fun (B,x) -> x) (fun x -> x#left) "> 0" s in
      receive (fun (C,x) -> x) s >>= fun (`msg(v,s)) ->
      close s;
      Lwt.return ()
    end else begin
      print_endline "<=0";
      let s = send (fun (B,x) -> x) (fun x -> x#right) false s in
      receive (fun (C,x) -> x) s >>= fun (`msg(v,s)) ->
      print_endline "t1 received from c";
      t1 s
    end

let rec t2 s : unit Lwt.t =
  let open Lwt in
  print_endline "t2";
  let s = send (fun (A,x) -> x) (fun x -> x#msg) 0 s in
  (* print_endline "t2 cont"; *)
  receive (fun (A,x) -> x) s >>= function
  | `left(v,s) ->
     let s = send (fun (C,x) -> x) (fun x->x#left) () s in
     close s;
     Lwt.return ()
  | `right(v,s) ->
     let s = send (fun (C,x) -> x) (fun x->x#right) () s in
     t2 s

let rec t3 s : unit Lwt.t =
  print_endline "t3";
  let open Lwt in
  receive (fun (B,x) -> x) s >>= function
  | `left(v,s) ->
     let s = send (fun (A,x)->x) (fun x->x#msg) 100 s in
     close s;
     Lwt.return ()
  | `right(w,s) ->
     let s = send (fun (A,x)->x) (fun x->x#msg) "abc" s in
     t3 s

  
let () =
  let g = mk_g () in (* never put g at toplevel (memory leaks otherwise)  *)
  Lwt_main.run (Lwt.join [t1 ((fst a).get g); t2 ((fst b).get g); t3 ((fst c).get g)])
