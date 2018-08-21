open Mpst.Base
open Mpst.Session3.MPST
open Lwt

let applebanana = {sender2=(fun (x,y)->object method apple=x method banana=y end);
                   receiver2=(function Left x -> `apple x | Right x -> `banana x)}

let mk_g () =
  (a -%%-> b)
      ~left:((a,b),
             (c --> b) msg @@
             finish)
      ~right:((a,b),
             (b --> a) msg @@
             (c --> b) msg @@
             finish)

let () =
  let g = mk_g () in
  begin try
      ignore (get_sess c g)
    with
      RoleNotEnabled ->
      print_endline "Exception RoleNotEnabled raised as expected"
  end;
  ()

let rec mk_g2 () =
  (a -%%-> b)
      ~left:((a,b),
             dummy_close c @@ (* stackoverflow occurs if absent *)
             loop mk_g2)
      ~right:((a,b),
             finish)

let () =
  let g = mk_g2 () in
  ignore (get_sess c g);
  print_endline "got endpoint at Role C successfully"

let rec mk_g3 () =
  (a -%%-> b)
      ~left:((a,b),
             (b --> c) msg @@
             finish)
      ~right:((a,b),
             dummy_receive c @@ (* don't use this: it cause choosing among indefinitely many promises *)
             loop mk_g3)

let rec t1 i s =
  if i >= 10 then (* will be stack overflow or segfault when > 1000000 *)
    let s = send B (fun x -> x#left) () s in
    close s;
    print_endline "t1 finished";
    Lwt.return ()
  else
    let s = send B (fun x -> x#right) i s in
    Lwt.return () >>= fun () ->
    t1 (i+1) s

let rec t2 s =
  receive A s >>= function
  | `left(i, s) ->
     let s = send C (fun x -> x#msg) () s in
     close s;
     print_endline "t2 finished";
     Lwt.return ()
  | `right(i, s) ->
     Printf.printf "%d\n" i;
     t2 s

let rec t3 s =
  receive B s >>= fun (`msg((),s)) ->
  close s;
  print_endline "t3 finished";
  Lwt.return ()

let () =
  let g = mk_g3 () in
  ignore (get_sess c g);
  print_endline "got endpoint at Role C successfully";
  Lwt_main.run @@ Lwt.join [t1 0 (get_sess a g); t2 (get_sess b g); t3 (get_sess c g)]
