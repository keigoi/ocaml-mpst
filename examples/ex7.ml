(* explicit connections *)
open Mpst.ThreeParty
let (>>=) = Lwt.(>>=)

module M = Marshal_example

let msg k = msg_ M.write M.read k
let left k = left_ M.write M.read k
let right k = right_ M.write M.read k
let leftright k = leftright_ M.write M.read k

let swap (a,b) = (b,a)

let mk_g () =
  let g =
    (b -!-> a) msg @@ (fun kba ->
    (a -!-> c) msg @@ (fun kac ->
    (c -%%-> a) (leftright (swap (kac)))
      ~l1:((c,a),
           (a --> b) (right (swap kba)) @@
           discon b a kba @@
           discon a c kac @@
           finish)
      ~l2:((c,a),
           (a --> b) (left (swap kba)) @@
           discon b a kba @@
           discon a c kac @@
           finish)
    ))
  in
  g

let kab = M.create_shmem_channel ()
let kac = M.create_shmem_channel ()

let ta s =
  M.shmem_accept kab >>= fun kb ->
  accept B kb s >>= fun (`msg((), s)) ->
  let kc = M.shmem_connect kac in
  let s = request C (fun x->x#msg) () kc s in
  begin
    receive C s >>= function
    | `left((), s) ->
       print_endline "ta: left";
       let s = send B (fun x->x#right) () s in
       Lwt.return s
    | `right((), s) ->
       print_endline "ta: right";
       let s = send B (fun x->x#left) () s in
       Lwt.return s
  end >>= fun s ->
  disconnect B M.shmem_disconnect s >>= fun s ->
  disconnect C M.shmem_disconnect s >>= fun s ->
  close s;
  print_endline "ta finished";
  Lwt.return ()



let tb s =
  let ka = M.shmem_connect kab in
  let s = request A (fun x->x#msg) () ka s in
  begin
    receive A s >>= function
    | `right((),s) ->
       print_endline "tb: right";
       Lwt.return s
    | `left((),s) ->
       print_endline "tb: left";
       Lwt.return s
  end >>= fun s ->
  disconnect A M.shmem_disconnect s >>= fun s ->
  close s;
  print_endline "tb finished";
  Lwt.return ()



let tc s =
  M.shmem_accept kac >>= fun ka ->
  accept A ka s >>= fun (`msg((),s)) ->
  begin
    if Random.bool () then begin
        print_endline "tc: select left";
        let s = send A (fun x->x#left) () s in
        Lwt.return s
      end else begin
        print_endline "tc: select right";
        let s = send A (fun x->x#right) () s in
        Lwt.return s
      end
  end >>= fun s ->
  disconnect A M.shmem_disconnect s >>= fun s ->
  close s;
  print_endline "tc finished";
  Lwt.return ()

let () = Random.self_init ()

let () =
  let g = mk_g () in
  Lwt_main.run (Lwt.join [ta (get_sess a g); tb (get_sess b g); tc (get_sess c g)])


(* loop example *)
let mk_g' () =
    (b -!-> a) msg @@ (fun kba ->
    (a -!-> c) msg @@ (fun kac ->
    let rec g =
      lazy begin
      (c -%%-> a) (leftright (swap (kac)))
        ~l1:((c,a),
             (a --> b) (right (swap kba)) @@
             discon b a kba @@
             discon a c kac @@
             finish)
        ~l2:((c,a),
             (a --> b) (left (swap kba)) @@
             loop_ g)
        end
    in
    Lazy.force g))

let rec ta' s =
  M.shmem_accept kab >>= fun kb ->
  accept B kb s >>= fun (`msg((), s)) ->
  let kc = M.shmem_connect kac in
  let s = request C (fun x->x#msg) () kc s in
  let rec loop s =
    receive C s >>= function
    | `left((), s) ->
       print_endline "ta: left";
       let s = send B (fun x->x#right) () s in
       Lwt.return s
    | `right((), s) ->
       print_endline "ta: right";
       let s = send B (fun x->x#left) () s in
       loop s
  in
  loop s>>= fun s ->
  disconnect B M.shmem_disconnect s >>= fun s ->
  disconnect C M.shmem_disconnect s >>= fun s ->
  close s;
  print_endline "ta finished";
  Lwt.return ()


let tb' s =
  let ka = M.shmem_connect kab in
  let s = request A (fun x->x#msg) () ka s in
  let rec loop s =
    receive A s >>= function
    | `right((),s) ->
       print_endline "tb: right";
       Lwt.return s
    | `left((),s) ->
       print_endline "tb: left";
       loop s
  in
  loop s >>= fun s ->
  disconnect A M.shmem_disconnect s >>= fun s ->
  close s;
  print_endline "tb finished";
  Lwt.return ()

let tc' s =
  M.shmem_accept kac >>= fun ka ->
  accept A ka s >>= fun (`msg((),s)) ->
  let rec loop s =
    if Random.bool () then begin
        print_endline "tc: select left";
        let s = send A (fun x->x#left) () s in
        Lwt.return s
      end else begin
        print_endline "tc: select right";
        let s = send A (fun x->x#right) () s in
        loop s
      end
  in
  loop s >>= fun s ->
  disconnect A M.shmem_disconnect s >>= fun s ->
  close s;
  print_endline "tc finished";
  Lwt.return ()

let () =
  print_endline "try calling mk_g'";
  let g = mk_g' () in
  print_endline "generated";
  Lwt_main.run (Lwt.join [ta' (get_sess a g); tb' (get_sess b g); tc' (get_sess c g)])
