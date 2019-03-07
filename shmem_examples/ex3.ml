(* simple loop *)
open Shmem.Session
open Shmem.Global
open Util


let ga = {role=`A; lens=FstOne}
let gb = {role=`B; lens=Next FstOne}
let gc = {role=`C; lens=Next (Next FstOne)}
let lv = Lazy.from_val
      
let finish = lv (ConsOne(lv (ProtOne Close),lv @@ ConsOne(lv (ProtOne Close),lv @@ ConsOne(lv (ProtOne Close), lv Nil))))

let (>>=) = Lwt.(>>=)

let mk_g () =
  let rec g =
    lazy
      begin
        (gb --> ga) msg @@
        (ga --> gc) msg @@
        (gc --> gb) msg @@
        lazy (Lazy.force (Lazy.force g))
      end
  in
  Lazy.force g

let a : [`A] = `A
let b : [`B] = `B
let c : [`C] = `C

let rec tA s =
  Lwt_io.print "tA\n" >>= fun () ->
  receive b s >>= fun (`msg((),s)) ->
  let s = send c (fun x->x#msg) () s in
  tA s

let rec tB s =
  Lwt_io.print "tB\n" >>= fun () ->
  let s = send a (fun x->x#msg) () s in
  receive c s >>= fun (`msg((),s)) ->
  tB s

let rec tC s =
  Lwt_io.print "tC\n" >>= fun () ->
  receive a s >>= fun (`msg((),s)) ->
  let s = send b (fun x->x#msg) () s in
  tC s

let () =
  let g = mk_g () in
  Lwt_main.run (Lwt.join [tA (get_sess ga g); tB (get_sess gb g); tC (get_sess gc g)])
