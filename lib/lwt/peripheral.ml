module LwtEvent : Mpst.S.EVENT
       with type 'a monad = 'a Lwt.t
       with type 'a event = unit -> 'a Lwt.t
  = struct
  type 'a monad = 'a Lwt.t
  type 'a event = unit -> 'a Lwt.t
  type 'a st = 'a Mstream.one Mstream.out * 'a Mstream.one Mstream.inp
  type 'a channel = {me:'a st; othr:'a st}

  let new_channel () =
    let ch1 = Mstream.create ()
    and ch2 = Mstream.create ()
    in
    {me=ch1; othr=ch2}
  let[@inline] receive {me=(_,inp); _} () = Mstream.receive inp
  let flip_channel {me=othr; othr=me} = {me; othr}
  let[@inline] send {othr=(out,_); _} v () = Mstream.send out v
  let[@inline] guard f = f ()
  let[@inline] sync f = f ()
  let always x () = Lwt.return x

  (* needs refactoring *)
  type 'a inp = 'a Mstream.one Mstream.inp
  let[@inline] inp {me=(_,inp); _} = inp
  let[@inline] receive_inp inp () = Mstream.receive inp
  let merge_inp = Mstream.merge_inp
  let wrap_inp = Mstream.wrap_inp
  let receive_list chs () =
    Lwt_list.map_p (fun {me=(_,inp);_} ->
        let x = Mstream.receive inp in
        x
      ) chs
  let receive_list_inp chs =
    failwith "receive_list_inp (lwt/peripheral.ml): not implemented"
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
end
