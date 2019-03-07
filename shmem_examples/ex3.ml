(* simple loop *)
open Shmem.Session
open Shmem.Global
open Util


let ga = {role=`A; lens=FstProt}
let gb = {role=`B; lens=Next FstProt}
let gc = {role=`C; lens=Next (Next FstProt)}
let lv = Lazy.from_val
      
let finish = lv (ConsProt(lv Close,lv @@ ConsProt(lv Close,lv @@ ConsProt(lv Close, lv Nil))))

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
  let lens_get a b = Lazy.force @@ lens_get a b in
  Lwt_main.run (Lwt.join [tA (lens_get ga.lens g); tB (lens_get gb.lens g); tC (lens_get gc.lens g)])
