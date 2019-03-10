open Explicit.Session
open Explicit.Global
open Explicit.Util.Labels

let (>>=) = Lwt.(>>=)
let cnt =
  let r = ref 1 in
  fun () -> let x= !r in r := x+1; x
type 'a stream = {st: 'a Lwt_stream.t; push: 'a option -> unit}
type dstream = {down: string Lwt_stream.t; up: string option -> unit;cnt:int; me:string}
type shmem_chan = Chan of dstream stream

let create_shmem_channel () =
  let st, push = Lwt_stream.create () in
  Chan {st; push}

let create_dstream () =
  let st1, push1 = Lwt_stream.create () in
  let st2, push2 = Lwt_stream.create () in
  let cnt = cnt () in
  {down=st1; up=push2; cnt=cnt; me=""}, {down=st2; up=push1; cnt=cnt; me=""}

let shmem_accept me (Chan {st}) =
  Lwt.bind (Lwt_stream.next st) (fun c1 ->
  Lwt.return {c1 with me})
    
let shmem_connect me (Chan {push}) =
  let c1, c2 = create_dstream () in
  push (Some c1);
  {c2 with me}

let shmem_disconnect {up} = Lwt.return (up None)

let marshal_flags = [Marshal.Closures]

let write c v =
     let str =
       begin
         try
           Marshal.to_string v marshal_flags
         with
         | e -> Printf.eprintf "fail at marshal\n%!"; raise e
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
           match f v with
           | Some v ->
             Lwt_stream.next c.down >>= fun _ ->
             Lwt.return v
           | None ->
             Lwt.choose []
         with
         | e -> Printf.eprintf "fail at unmarshal\n%!"; raise e
       end

type 'v m = Msg of 'v | Left of 'v | Right of 'v | Deleg of 'v

class marshal : [dstream,dstream] standard =
  object
    method ch_msg: 'v. (_,_,'v) channel =
      {sender=(fun t v -> write t (Msg(v)));
       receiver=(fun t -> read t (function Msg(v) -> Some v | _ -> None))}
    method ch_left: 'v. (_,_,'v) channel =
      {sender=(fun t v -> write t (Left(v)));
       receiver=(fun t -> read t (function Left(v) -> Some v | _ -> None))}
    method ch_right: 'v. (_,_,'v) channel =
      {sender=(fun t v -> write t (Right(v)));
       receiver=(fun t -> read t (function Right(v) -> Some v | _ -> None))}
    method ch_deleg: 'v. (_,_,'v) channel =
      {sender=(fun t v -> write t (Deleg(v)));
       receiver=(fun t -> read t (function Deleg(v) -> Some v | _ -> None))}
  end
