(* droppping liveness at some participant *)
open Mpst.ThreeParty
open Mpst.ThreeParty.Shmem
let (>>=) = Lwt.(>>=)

let stop =
  {make_channel=make_shmem_channel;
   select_label=(fun f -> object method stop=f end);
   offer_label=(fun l -> `stop l)
  }

let mk_g () =
  let rec g =
    lazy begin
        choice_at a left_or_right
          (a, (a --> b) left @@
              (b --> c) stop @@
              finish)
          (a, (a --> b) right @@
              dummy_receive c @@ (* no liveness at c*)
              loop g)
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
    let s = send b (fun x -> x#left) () s in
    close s;
    print_endline "tA finished";
    Lwt.return ()
  end else begin
      Printf.printf "tA %d\n" i;
      let s = send b (fun x -> x#right) i s in
      Lwt.return () >>= fun () ->
      tA (i+1) s
    end


let rec tB s =
  print_endline "tB";
  receive a s >>= function
  | `left(i, s) ->
     let s = send c (fun x -> x#stop) () s in
     close s;
     print_endline "tB finished";
     Lwt.return ()
  | `right(i, s) ->
     Printf.printf "tB %d\n" i;
     tB s

let rec tC s =
  print_endline "tC\n";
  receive b s >>= fun (`stop((),s)) ->
  close s;
  print_endline "tC finished";
  Lwt.return ()

let () =
  let g = mk_g () in
  Lwt_main.run @@ Lwt.join [tB (get_sess b g); tA 0 (get_sess a g); tC (get_sess c g)]
