(* create your own labels (see util.ml for other examples) *)
open Mpst.ThreeParty
open Mpst.ThreeParty.Shmem
let (>>=) = Lwt.(>>=)

let applebanana () =
  let st, push = Lwt_stream.create () in
  Labels.mklabel2
    (fun f g -> object method left=f method right=g end)
    (fun x -> `left x) (fun x -> `right x)
    (fun _ v -> push (Some v)) (fun _ -> Lwt_stream.next st)
    dummyconn

let mk_g () =
  (a -%%-> b) (applebanana ())
      ~l1:((a,b),
             (c --> b) (msg ()) @@
             finish)
      ~l2:((a,b),
             (b --> a) (msg ()) @@
             (c --> b) (msg ()) @@
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
  let rec g =
    lazy
      begin
        (a -%%-> b) (applebanana ())
            ~l1:((a,b),
                   dummy_close c @@ (* stackoverflow occurs if absent *)
                   loop_ g)
            ~l2:((a,b),
                 finish)
      end
  in
  Lazy.force g

let () =
  let g = mk_g2 () in
  ignore (get_sess c g);
  print_endline "got endpoint at Role C successfully"

let mk_g3 () =
  let rec g =
    lazy
      begin
        (a -%%-> b) (applebanana ())
            ~l1:((a,b),
                   (b --> c) (msg ()) @@
                   finish)
            ~l2:((a,b),
                   dummy_receive c @@
                   loop_ g)
      end
  in
  Lazy.force g

let rec tA i s =
  if i >= 10 then (* will be stack overflow or segfault when > 1000000 *)
    let s = send B (fun x -> x#left) () s in
    close s;
    print_endline "tA finished";
    Lwt.return ()
  else
    let s = send B (fun x -> x#right) i s in
    Lwt.return () >>= fun () ->
    tA (i+1) s

let rec tB s =
  receive A s >>= function
  | `left(i, s) ->
     let s = send C (fun x -> x#msg) () s in
     close s;
     print_endline "tB finished";
     Lwt.return ()
  | `right(i, s) ->
     Printf.printf "%d\n" i;
     tB s

let rec tC s =
  receive B s >>= fun (`msg((),s)) ->
  close s;
  print_endline "tC finished";
  Lwt.return ()

let () =
  let g = mk_g3 () in
  ignore (get_sess c g);
  print_endline "got endpoint at Role C successfully";
  Lwt_main.run @@ Lwt.join [tA 0 (get_sess a g); tB (get_sess b g); tC (get_sess c g)]
