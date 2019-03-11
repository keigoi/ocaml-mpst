(* droppping liveness at some participant *)
open Mpst_shmem.Global
open Mpst_shmem.Session
open Mpst_shmem.Util
let (>>=) = Lwt.(>>=)

let a = {role=`A; lens=Fst}
let b = {role=`B; lens=Next Fst}
let c = {role=`C; lens=Next (Next Fst)}

let finish = one @@ one @@ one @@ nil

let stop =
  {select_label=(fun f -> object method stop=f end);
   offer_label=(fun l -> `stop l)
  }

let mk_g () =
  let rec g =
    lazy begin
        (a --> b) msg @@ (* FIXME: fails if this line is removed *)
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
  print_endline "global generated";
  let _ = get_sess a g in
  print_endline "endpoint A generated";
  let _ = get_sess b g in
  print_endline "endpoint B generated";
  let _ = get_sess c g in
  print_endline "endpoint C generated";
  ()

let rec tA i s =
  Printf.printf "A:%d\n" i;
  Lwt_unix.sleep 1.0 >>= fun () ->
  let s = send `B (fun x ->x#msg) () s in
  if i >= 10 then begin
    let s = send `B (fun x -> x#left) () s in
    close s;
    print_endline "tA finished";
    Lwt.return ()
  end else begin
      Printf.printf "tA %d\n" i;
      let s = send `B (fun x -> x#right) i s in
      Lwt.return () >>= fun () ->
      tA (i+1) s
    end


let rec tB s =
  print_endline "tB";
  receive `A s >>= fun (`msg((),s)) ->
  receive `A s >>= function
  | `left(i, s) ->
     let s = send `C (fun x -> x#stop) () s in
     close s;
     print_endline "tB finished";
     Lwt.return ()
  | `right(i, s) ->
     Printf.printf "tB %d\n" i;
     tB s

let rec tC s =
  print_endline "tC\n";
  receive `B s >>= fun (`stop((),s)) ->
  close s;
  print_endline "tC finished";
  Lwt.return ()

let () =
  let g = mk_g () in
  Lwt_main.run @@ Lwt.join [tB (get_sess b g); tA 0 (get_sess a g); tC (get_sess c g)]
