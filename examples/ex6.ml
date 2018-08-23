(* droppping liveness at some participant *)
open Mpst.ThreeParty
open Mpst.ThreeParty.Shmem
let (>>=) = Lwt.(>>=)

let mk_g () =
  let rec g =
  lazy begin
  (a -%%-> b) (leftright ())
      ~l1:((a,b),
             (b --> c) (msg ()) @@
             finish)
      ~l2:((a,b),
             dummy_receive c @@ (* no liveness at c*)
             loop_ g)
    end
  in
  Lazy.force g

let () =
  let g = mk_g () in
  let _ = get_sess a g in
  let _ = get_sess b g in
  let _ = get_sess c g in
  print_endline "endpoints generated"

let rec tA i s =
  Lwt_unix.sleep 1.0 >>= fun () ->
  if i >= 10 then begin
    let s = send B (fun x -> x#left) () s in
    close s;
    print_endline "tA finished";
    Lwt.return ()
  end else begin
      Printf.printf "tA %d\n" i;
      let s = send B (fun x -> x#right) i s in
      Lwt.return () >>= fun () ->
      tA (i+1) s
    end


let rec tB s =
  print_endline "tB";
  receive A s >>= function
  | `left(i, s) ->
     let s = send C (fun x -> x#msg) () s in
     close s;
     print_endline "tB finished";
     Lwt.return ()
  | `right(i, s) ->
     Printf.printf "tB %d\n" i;
     tB s

let rec tC s =
  print_endline "tC\n";
  receive B s >>= fun (`msg((),s)) ->
  close s;
  print_endline "tC finished";
  Lwt.return ()

let () =
  let g = mk_g () in
  Lwt_main.run @@ Lwt.join [tB (get_sess b g); tA 0 (get_sess a g); tC (get_sess c g)]
