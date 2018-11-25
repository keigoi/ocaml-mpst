open Mpst.ThreeParty
let (>>=) = Lwt.(>>=)

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

let shmem_disconnect {up} = Lwt.return (up None)

let marshal_flags = []

let write c v =
     let str =
       begin
         try
           Marshal.to_string v marshal_flags
         with
         | e -> Printf.eprintf "fail at marshal"; raise e
       end
     in
     c.up (Some str)

let read c f = 
  Lwt_stream.peek c.down >>= fun s ->
  match s with
  | None ->
     assert false
  | Some str ->
       begin try
           let v = Marshal.from_string str 0 in
           if f v then begin
             Lwt_stream.next c.down >>= fun _ ->
             Lwt.return v
           end else
             Lwt.choose []
         with
         | e -> Printf.eprintf "fail at unmarshal"; raise e
       end
