(* explicit connections *)
open Mpst.ThreeParty
let (>>=) = Lwt.(>>=)

module M = Marshal_example

let mk_g (m : ('k1,'k2) #standard) =
  let g =
    ((b,b) -!-> (a,a)) (msg m) @@
    ((a,a) -!-> (c,c)) (msg m) @@
    choice_at c left_or_right
      (c, (c --> a) (left m) @@
          (a --> b) (right m) @@
          discon a b @@
          discon a c @@
          finish)
      (c, (c --> a) (right m) @@
          (a --> b) (left m) @@
          discon a b @@
          discon a c @@
          finish)
  in
  g

let kab = M.create_shmem_channel ()
let kac = M.create_shmem_channel ()

let ta s =
  M.shmem_accept kab >>= fun kb ->
  accept b kb s >>= fun (`msg((), s)) ->
  let kc = M.shmem_connect kac in
  let s = request c (fun x->x#msg) () kc s in
  begin
    receive c s >>= function
    | `left((), s) ->
       print_endline "ta: left";
       let s = send b (fun x->x#right) () s in
       Lwt.return s
    | `right((), s) ->
       print_endline "ta: right";
       let s = send b (fun x->x#left) () s in
       Lwt.return s
  end >>= fun s ->
  let s = disconnect b s in
  let s = disconnect c s in
  close s;
  print_endline "ta finished";
  Lwt.return ()



let tb s =
  let ka = M.shmem_connect kab in
  let s = request a (fun x->x#msg) () ka s in
  begin
    receive a s >>= function
    | `right((),s) ->
       print_endline "tb: right";
       Lwt.return s
    | `left((),s) ->
       print_endline "tb: left";
       Lwt.return s
  end >>= fun s ->
  let s = disconnect a s in
  close s;
  print_endline "tb finished";
  Lwt.return ()



let tc s =
  M.shmem_accept kac >>= fun ka ->
  accept a ka s >>= fun (`msg((),s)) ->
  begin
    if Random.bool () then begin
        print_endline "tc: select left";
        let s = send a (fun x->x#left) () s in
        Lwt.return s
      end else begin
        print_endline "tc: select right";
        let s = send a (fun x->x#right) () s in
        Lwt.return s
      end
  end >>= fun s ->
  let s = disconnect a s in
  close s;
  print_endline "tc finished";
  Lwt.return ()

let () = Random.self_init ()

let () =
  let g = mk_g (new M.marshal) in
  Lwt_main.run (Lwt.join [ta (get_sess a g); tb (get_sess b g); tc (get_sess c g)])

(* loop example *)
let mk_g' m =
    ((b,b) -!-> (a,a)) (msg m) @@
    ((a,a) -!-> (c,c)) (msg m) @@
    let rec g =
      lazy begin
      choice_at c left_or_right
        (c, (c --> a) (left m) @@
            (a --> b) (right m) @@
            discon b a @@
            discon a c @@
            finish)
        (c, (c --> a) (right m) @@
            (a --> b) (left m) @@
            loop g)
        end
    in
    Lazy.force g

let rec ta' s =
  M.shmem_accept kab >>= fun kb ->
  accept b kb s >>= fun (`msg((), s)) ->
  let kc = M.shmem_connect kac in
  let s = request c (fun x->x#msg) () kc s in
  let rec loop s =
    receive c s >>= function
    | `left((), s) ->
       print_endline "ta: left";
       let s = send b (fun x->x#right) () s in
       Lwt.return s
    | `right((), s) ->
       print_endline "ta: right";
       let s = send b (fun x->x#left) () s in
       loop s
  in
  loop s >>= fun s ->
  let s = disconnect b s in
  let s = disconnect c s in
  close s;
  print_endline "ta' finished";
  Lwt.return ()


let tb' s =
  let ka = M.shmem_connect kab in
  let s = request a (fun x->x#msg) () ka s in
  let rec loop s =
    receive a s >>= function
    | `right((),s) ->
       print_endline "tb: right";
       Lwt.return s
    | `left((),s) ->
       print_endline "tb: left";
       loop s
  in
  loop s >>= fun s ->
  let s = disconnect a s in
  close s;
  print_endline "tb' finished";
  Lwt.return ()

let tc' s =
  M.shmem_accept kac >>= fun ka ->
  accept a ka s >>= fun (`msg((),s)) ->
  let rec loop s =
    if Random.bool () then begin
        print_endline "tc: select left";
        let s = send a (fun x->x#left) () s in
        Lwt.return s
      end else begin
        print_endline "tc: select right";
        let s = send a (fun x->x#right) () s in
        loop s
      end
  in
  loop s >>= fun s ->
  let s = disconnect a s in
  close s;
  print_endline "tc' finished";
  Lwt.return ()

let () =
  print_endline "try calling mk_g'";
  let g = mk_g' (new M.marshal) in
  print_endline "generated";
  Lwt_main.run (Lwt.join [ta' (get_sess a g); tb' (get_sess b g); tc' (get_sess c g)])
