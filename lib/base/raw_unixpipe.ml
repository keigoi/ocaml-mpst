open Base


let (>>=) = Lwt.(>>=)
(* type conn = {out:Obj.t -> unit; inp:Obj.t Lwt_stream.t; close:unit -> unit} *)
(* let create_ ~inp:fdin ~out:fdout =
 *   let inpchan = Unix.in_channel_of_descr fdin in
 *   let outchan = Unix.out_channel_of_descr fdout in
 *   {out=(fun v -> output_value outchan v; flush outchan);
 *    inp=
 *      Lwt_stream.from_direct (fun () ->
 *          print_endline "receive";
 *          let v = input_value inpchan in
 *          print_endline "receive done";
 *          Some(v)
 *        );
 *    close=(fun () -> Unix.close fdin; Unix.close fdout)
 *   }
 * 
 * let create () =
 *   let p_in, c_out = Unix.pipe () in
 *   let c_in, p_out = Unix.pipe () in
 *   create_ ~inp:p_in ~out:p_out,
 *   create_ ~inp:c_in ~out:c_out *)

type conn = {out:Obj.t -> unit; inp:unit -> Obj.t Lwt.t; mutable buf:Obj.t option; close:unit -> unit}
let create_ ~inp:fdin ~out:fdout =
  let inpchan = Lwt_io.of_fd Lwt_io.input fdin in
  let outchan = Lwt_io.of_fd Lwt_io.output fdout in
  {out=(fun v -> ignore (Lwt_io.write_value outchan v >>= fun _ -> Lwt_io.flush outchan));
   inp=(fun () ->
     Lwt_io.read_value inpchan
   );
   buf=None;
   close=(fun () -> ignore (Lwt_io.close inpchan >>= fun _ -> Lwt_io.close outchan))
  }

let create () =
  let p_in, c_out = Lwt_unix.pipe () in
  let c_in, p_out = Lwt_unix.pipe () in
  create_ ~inp:p_in ~out:p_out,
  create_ ~inp:c_in ~out:c_out

let write wrap {out; _} v =
  out (Obj.repr (wrap (Obj.repr v)))
  
let try_read unwrap ({inp; buf; _} as r) =
  match buf with
  | None ->
     inp () >>= fun v ->
     begin match unwrap (Obj.obj v) with
     | Some(v) ->
        Lwt.return (Obj.obj v)
     | None ->
        r.buf <- Some(v);
        Lwt.fail ReceiveFail
     end
  | Some(v) ->
     begin match unwrap (Obj.obj v) with
     | Some(v) ->
        r.buf <- None;
        Lwt.return (Obj.obj v)
     | None ->
        Lwt.fail ReceiveFail
     end

          
