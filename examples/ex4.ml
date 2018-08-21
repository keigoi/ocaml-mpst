open Mpst.Base
open Mpst.Session3.MPST
open Lwt
   
let applebanana = {sender2=(fun (x,y)->object method apple=x method banana=y end);
                 receiver2=(function Left x -> `apple x | Right x -> `banana x)}

let rec mk_g () =
  (b --> a) msg @@
    (a -!%%-> b) applebanana
      ~l1:((a,b),
             (b --> c) left @@
             (c --> a) msg @@
             finish)
      ~l2:((a,b),
             (b --> c) right @@
             (c --> a) msg @@
             loop mk_g)

let rec t1 s : unit Lwt.t =
  let open Lwt in
  print_endline "t1";
  receive B s >>= fun (`msg(v,s)) ->
  print_endline "t1 cont";
  if v > 0 then begin
      print_endline ">0";
      let s = send B (fun x -> x#apple) "> 0" s in
      receive C s >>= fun (`msg(v,s)) ->
      close s;
      Lwt.return ()
    end else begin
      print_endline "<=0";
      let s = send B (fun x -> x#banana) false s in
      receive C s >>= fun (`msg(v,s)) ->
      print_endline "t1 received from c";
      t1 s
    end

let rec t2 s : unit Lwt.t =
  let open Lwt in
  print_endline "t2";
  let s = send A (fun x -> x#msg) 0 s in
  receive A s >>= function
  | `apple(v,s) ->
     let s = send C (fun x->x#left) () s in
     close s;
     Lwt.return ()
  | `banana(v,s) ->
     let s = send C (fun x->x#right) () s in
     t2 s

let rec t3 s : unit Lwt.t =
  print_endline "t3";
  let open Lwt in
  receive B s >>= function
  | `left(v,s) ->
     let s = send A (fun x->x#msg) 100 s in
     close s;
     Lwt.return ()
  | `right(w,s) ->
     let s = send A (fun x->x#msg) "abc" s in
     t3 s

  
let () =
  let g = mk_g () in (* never put g at toplevel (memory leaks otherwise)  *)
  Lwt_main.run (Lwt.join [t1 (get_sess a g); t2 (get_sess b g); t3 (get_sess c g)])


let rec mk_g2 () =
  (b --> a) msg @@
  (a -%%-> b)
      ~left:((a,b),
             (a -%%-> b)
               ~left:((a,b),
                      (b --> c) left @@
                      (c --> a) msg @@
                      finish)
               ~right:((a,b),
                      (b --> c) middle @@
                      (c --> a) msg @@
                      loop mk_g))
      ~right:((a,b),
             (b --> c) right @@
             (c --> a) msg @@
             loop mk_g)
