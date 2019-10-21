open Mpst.Base
module LwtEvent : Mpst.S.EVENT
       with type 'a monad = 'a Lwt.t
       with type 'a event = unit -> 'a Lwt.t
  = struct
  type 'a monad = 'a Lwt.t
  type 'a event = unit -> 'a Lwt.t
  type 'a stream = 'a one Mstream.out * 'a one Mstream.inp
  type 'a channel = {me:'a stream; othr:'a stream}

  let new_channel () =
    let ch1 = Mstream.create_one ()
    and ch2 = Mstream.create_one ()
    in
    {me=ch1; othr=ch2}
  let[@inline] receive {me=(_,inp); _} () = Mstream.receive inp
  let flip_channel {me=othr; othr=me} = {me; othr}
  let[@inline] send {othr=(out,_); _} v () = Mstream.send out v

  let[@inline] sync f = f ()

  type 'a st = 'a Mstream.st
  type 'a inp = 'a Mstream.inp
  type 'a out = 'a Mstream.out

  let create_st = Mstream.create
  let wrap = Mstream.wrap
  let wrap_scatter = Mstream.wrap_scatter
  let wrap_gather = Mstream.wrap_gather

  let[@inline] send_st out v () = Mstream.send out v
  let[@inline] sendmany_st out v () = Mstream.send_many out v
  let[@inline] receive_st inp () = Mstream.receive inp
  let[@inline] receivemany_st inp () = Mstream.receive_many inp

  let merge_out = Mstream.merge_out
  let merge_inp = Mstream.merge_inp
end


module LwtSerial : Mpst.S.SERIAL
       with type 'a monad = 'a Lwt.t and   type in_channel = Lwt_io.input_channel and type out_channel = Stdlib.out_channel
  = struct
  type 'a monad = 'a Lwt.t
  type in_channel = Lwt_io.input_channel
  type out_channel = Stdlib.out_channel
  let pipe () =
    let inp,out = Unix.pipe () in
    Lwt_io.of_unix_fd Lwt_io.input inp, Unix.out_channel_of_descr out
  let input_value ch =
    Lwt_io.read_value ch
  let input_tagged =
    input_value
  let output_value ch v =
    (* use Stdlib.output_value.
     * Lwt_io.write_value is much slower, as it writes to an intermediate buffer *)
    Lwt_preemptive.detach (fun () ->
                  Stdlib.output_value ch v;
                  Stdlib.flush ch) ()
  let output_tagged =
    output_value
  let flush _ch =
    Lwt.return ()
  let input_value_list chs =
    let rec loop acc = function
      | [] -> Lwt.return (List.rev acc)
      | ch::chs ->
         Lwt.bind (Lwt_io.read_value ch) (fun v ->
             loop (v::acc) chs)
    in loop [] chs

  let close_in = Lwt_io.close
  let close_out ch = close_out ch; Lwt.return_unit

  let fork_child f =
    let pid = Lwt_unix.fork () in
    if pid = 0 then begin
        (f ():unit);
        exit 0;
      end
    else
      pid
end
module Lwt = struct
  include Lwt
  let mapM = Lwt_list.map_p
  let iteriM = Lwt_list.iteri_p
  let yield = Lwt_main.yield

  type mutex = Lwt_mutex.t
  let create_mutex = Lwt_mutex.create
  let lock = Lwt_mutex.lock
  let unlock = Lwt_mutex.unlock
end
