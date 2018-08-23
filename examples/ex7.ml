open Mpst.Scribble_connect.MPST
open Lwt

type 'a stream = {st: 'a Lwt_stream.t; push: 'a option -> unit}
type dstream = {down: string Lwt_stream.t; up: string option -> unit}
type shmem_chan = Chan of dstream stream

let create_shmem_channel () =
  let st, push = Lwt_stream.create () in
  Chan {st; push}

let create_dstream () =
  let st1, push1 = Lwt_stream.create () in
  let st2, push2 = Lwt_stream.create () in
  {down=st1; up=push2}, {down=st2; up=push1}

let shmem_accept (Chan {st}) = Lwt_stream.next st
let shmem_connect (Chan {push}) =
  let c1, c2 = create_dstream () in
  push (Some c1);
  c2

let shmem_disconnect {up} = up None

let marshal_flags = []

let write : 'a. (_, dstream) conn -> 'a -> unit = function
  | {conn=Some{up}} -> fun v -> up (Some (Marshal.to_string v marshal_flags))
  | {conn=None} -> failwith "write: disconnected"

let read : 'a. (_, dstream) conn -> 'a Lwt.t = function
  | {conn=Some{down}} -> Lwt.map (fun str -> Marshal.from_string str 0) (Lwt_stream.next down)
  | {conn=None} -> failwith "read: disconnected"

let msg k = msg_ write read k
let left k = left_ write read k
let right k = right_ write read k

let swap (a,b) = (b,a)

let g =
  (b -!-> a) msg @@ (fun kba ->
  (a -!-> c) msg @@ (fun kac ->
  (c --> a) (left (swap kac)) @@
  (a --> b) (right (swap kba)) @@
  discon b a kba @@
  discon a c kac @@
  finish))

let kab = create_shmem_channel ()
let kac = create_shmem_channel ()

let ta s =
  shmem_accept kab >>= fun kb ->
  accept B kb s >>= fun (`msg((), s)) ->
  let kc = shmem_connect kac in
  let s = request C (fun x->x#msg) () kc s in
  receive C s >>= fun (`left((), s)) ->
  let s = send B (fun x->x#right) () s in
  let s = disconnect B shmem_disconnect s in
  let s = disconnect C shmem_disconnect s in
  close s;
  Lwt.return ()

let tb s =
  let ka = shmem_connect kab in
  let s = request A (fun x->x#msg) () ka s in
  receive A s >>= fun (`right((),s)) ->
  let s = disconnect A shmem_disconnect s in
  close s;
  Lwt.return ()

let tc s =
  shmem_accept kac >>= fun ka ->
  accept A ka s >>= fun (`msg((),s)) ->
  let s = send A (fun x->x#left) () s in
  let s = disconnect A shmem_disconnect s in
  close s;
  Lwt.return ()

let () =
  Lwt_main.run (Lwt.join [ta (get_sess a g); tb (get_sess b g); tc (get_sess c g)])
