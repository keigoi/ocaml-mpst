module LwtEvent : Mpst.S.EVENT
       with type 'a monad = 'a Lwt.t
       with type 'a event = unit -> 'a Lwt.t
  = struct
  type 'a monad = 'a Lwt.t
  type 'a event = unit -> 'a Lwt.t
  type 'a st = {write: 'a option -> unit; read:'a Lwt_stream.t}
  type 'a channel = {me:'a st; othr:'a st}

  let new_channel () =
    let r1, w2 = Lwt_stream.create ()
    and r2, w1 = Lwt_stream.create ()
    in
    {me={write=w1;read=r1}; othr={write=w2;read=r2}}
  let receive {me={read}; _} () = Lwt_stream.next read
  let flip_channel {me=othr; othr=me} = {me; othr}
  let send {me={write; _}; _} v () = write (Some v); Lwt.return_unit
  let guard f = f ()
  let sync f = f ()
  let choose xs = fun () -> Lwt.choose (List.map (fun f -> f ()) xs)
  let wrap e f = fun () -> Lwt.map f (e ())
  let always x () = Lwt.return x
  let receive_list chs () =
    Lwt_list.map_p (fun {me={read;_};_} ->
        let x = Lwt_stream.next read in
        x
      ) chs
end

module LwtSerial : Mpst.S.SERIAL
       with type 'a monad = 'a Lwt.t and   type in_channel = Lwt_io.input_channel and type out_channel = Lwt_io.output_channel
  = struct
  type 'a monad = 'a Lwt.t
  type in_channel = Lwt_io.input_channel
  type out_channel = Lwt_io.output_channel
  let pipe () =
    let inp,out = Lwt_unix.pipe () in
    Lwt_io.of_fd Lwt_io.input inp, Lwt_io.of_fd Lwt_io.output out
  let input_value ch =
    Lwt_io.read_value ch
  let input_tagged =
    input_value
  let output_value ch v =
    Lwt_io.write_value ch v
  let output_tagged =
    output_value
  let flush =
    Lwt_io.flush
  let input_value_list chs =
    let rec loop acc = function
      | [] -> Lwt.return (List.rev acc)
      | ch::chs ->
         Lwt.bind (Lwt_io.read_value ch) (fun v ->
             loop (v::acc) chs)
    in loop [] chs
end
module Lwt = struct
  include Lwt
  let mapM = Lwt_list.map_p
  let iteriM = Lwt_list.iteri_p
end
