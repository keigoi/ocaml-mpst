(* simple loop w/ multicasts *)
open Mpst_lwt
let (>>=) = Lwt.(>>=)

let prot () =
  fix @@ fun t ->
      begin
        (gather b a) msg @@
        (a --> c) msg @@
        (scatter c b) msg @@
        t
      end


let rec tA s i =
  Lwt_io.print "tA\n" >>= fun () ->
  receive s#role_B >>= fun (`msg(_,s)) ->
  send s#role_C#msg () >>= fun s ->
  Lwt_main.yield () >>= fun () ->
  if i > 10 then
    exit 0
  else
    tA s (i+1)

let rec tB ss =
  ss |>
    List.mapi (fun i s ->
        let rec loop s =
          Lwt_io.print (Printf.sprintf "tB(%d)\n" i) >>= fun () ->
          send s#role_A#msg () >>= fun s ->
          receive s#role_C >>= fun (`msg(_,s)) ->
          Lwt_main.yield () >>= fun () ->
          loop s in
        loop s)

let rec tC s =
  Lwt_io.print "tC\n" >>= fun () ->
  receive s#role_A >>= fun (`msg((),s)) ->
  sendmany s#role_B#msg (fun _ -> ()) >>= fun s ->
  tC s

let () =
  let g = gen_mult [1;10;1] (prot ()) in
  Lwt_main.run (Lwt.join [tA (get_ep a g) 0; Lwt.join (tB (get_ep_list b g)); tC (get_ep c g)])
