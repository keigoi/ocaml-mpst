(* create your own labels (see util.ml for other examples) *)
open Mpst_shmem.Global
open Mpst_shmem.Session
open Mpst_shmem.Util
let (>>=) = Lwt.(>>=)

let a = {role=`A; lens=Fst}
let b = {role=`B; lens=Next Fst}
let c = {role=`C; lens=Next (Next Fst)}

let finish = one @@ one @@ one @@ nil

let apple =
  {select_label=(fun x -> object method apple=x end);
   offer_label=(fun x -> `apple(x))}

let banana =
  {select_label=(fun x -> object method banana=x end);
   offer_label=(fun x -> `banana(x))}

let apple_or_banana =
  {label_merge=(fun a b -> object method apple=a#apple method banana=b#banana end)}

let mk_g () =
  choice_at a apple_or_banana
      (a, (a --> b) apple @@
          (c --> b) msg @@
          finish)
      (a, (a --> b) banana @@
          (b --> a) msg @@
          (c --> b) msg @@
          finish)

let () =
  let g = mk_g () in
  begin try
      ignore (get_sess c g)
    with
      Mpst_base.RoleNotEnabled ->
      print_endline "Exception RoleNotEnabled raised as expected"
  end;
  ()

let rec mk_g2 () =
  let rec g =
    lazy
      begin
        choice_at a apple_or_banana
            (a, (a --> b) apple @@
                dummy_close c @@ (* stackoverflow occurs if absent *)
                loop g)
            (a, (a --> b) banana @@
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
        choice_at a apple_or_banana
          (a, (a --> b) apple @@
              (b --> c) msg @@
              finish)
          (a, (a --> b) banana @@
              dummy_receive c @@
              loop g)
      end
  in
  Lazy.force g

let rec tA i s =
  if i >= 10 then (* will be stack overflow or segfault when > 1000000 *)
    let s = send `B (fun x -> x#apple) () s in
    close s;
    print_endline "tA finished";
    Lwt.return ()
  else
    let s = send `B (fun x -> x#banana) i s in
    Lwt.return () >>= fun () ->
    tA (i+1) s

let rec tB s =
  receive `A s >>= function
  | `apple(i, s) ->
     let s = send `C (fun x -> x#msg) () s in
     close s;
     print_endline "tB finished";
     Lwt.return ()
  | `banana(i, s) ->
     Printf.printf "%d\n" i;
     tB s

let rec tC s =
  receive `B s >>= fun (`msg((),s)) ->
  close s;
  print_endline "tC finished";
  Lwt.return ()

let () =
  let g = mk_g3 () in
  ignore (get_sess c g);
  print_endline "got endpoint at Role C successfully";
  Lwt_main.run @@ Lwt.join [tA 0 (get_sess a g); tB (get_sess b g); tC (get_sess c g)]
