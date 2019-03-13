open Base

type conn = {out:Obj.t -> unit; inp:Obj.t Lwt_stream.t; close:unit -> unit}

let (>>=) = Lwt.(>>=)

let create_ ~inp ~out =
  {out=(fun v -> out (Some v));
   inp=inp;
   close=(fun () -> ())
  }

let create () =
  let p_in, c_out = Lwt_stream.create () in
  let c_in, p_out = Lwt_stream.create () in
  create_ ~inp:p_in ~out:p_out,
  create_ ~inp:c_in ~out:c_out

let write wrap {out; _} v =
  out (Obj.repr (wrap (Obj.repr v)))
  
let try_read unwrap {inp; _} =
  Lwt_stream.peek inp >>= function
  | None -> Lwt.fail (Failure "end of stream")
  | Some(v) -> 
     match unwrap (Obj.obj v) with
     | Some(v) ->
        Lwt_stream.next inp >>= fun _ ->
        Lwt.return (Obj.obj v)
     | None ->
        Lwt.fail ReceiveFail
