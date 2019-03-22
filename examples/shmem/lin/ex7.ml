(* simple loop w/ multicasts *)
open Mpst_shmem.Session
open Mpst_shmem.Global
open Mpst_shmem.Util
let (>>=) = Lwt.(>>=)

let a = {role=`A; lens=Fst}
let b = {role=`B; lens=Next Fst}
let c = {role=`C; lens=Next (Next Fst)}

let mk_g () =
  let rec g =
    lazy
      begin
        (b >>-- a) msg @@
        (a --> c) msg @@
        (c -->> b) msg @@
        many_at_ b 10 @@ loop g
      end
  in
  Lazy.force g

let rec tA s =
  Lwt_io.print "tA\n" >>= fun () ->
  receive `B s >>= fun (`msg(_,s)) ->
  let s = send `C (fun x->x#msg) () s in
  tA s

let rec tB ss =
  ss |>
    List.mapi (fun i s ->
        let rec loop s =
          Lwt_io.print (Printf.sprintf "tB(%d)\n" i) >>= fun () ->
          let s = send `A (fun x->x#msg) () s in
          receive `C s >>= fun (`msg((),s)) ->
          loop s in
        loop s)

let rec tC s =
  Lwt_io.print "tC\n" >>= fun () ->
  receive `A s >>= fun (`msg((),s)) ->
  let s = send `B (fun x->x#msg) (fun _ -> ()) s in
  tC s

let () =
  let g = mk_g () in
  Lwt_main.run (Lwt.join [tA (get_sess a g); Lwt.join (tB (get_sess_many b g)); tC (get_sess c g)])
