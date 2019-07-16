
module AsyncEvent : Mpst.S.EVENT
       with type 'a monad = 'a Async.Deferred.t
       with type 'a event = unit -> 'a Async.Deferred.t
  = struct
  open Async
  type 'a monad = 'a Deferred.t
  type 'a event = unit -> 'a Deferred.t
  type 'a st = {read: 'a Pipe.Reader.t; write: 'a Pipe.Writer.t}
  type 'a channel = {me:'a st; othr:'a st}

  let new_channel () =
    let r1, w2 = Pipe.create ()
    and r2, w1 = Pipe.create ()
    in
    {me={write=w1;read=r1}; othr={write=w2;read=r2}}
  let receive {me={read}; _} () =
    Deferred.map (Pipe.read read) (function
    | `Ok(v) -> v
    | `Eof -> failwith "mpst_async: pipe eof")
  let flip_channel {me=othr; othr=me} = {me; othr}
  let send {me={write; _}; _} v () =
    Pipe.write write v
  let guard f = f ()
  let sync f = f ()
  let choose xs = fun () -> Deferred.any (List.map (fun f -> f ()) xs)
  let wrap e f () = Deferred.map ~f (e ())
  let always x () = Deferred.return x
  let receive_list chs () =
    Deferred.List.map chs (fun ch -> receive ch ())
end

module AsyncSerial : Mpst.S.SERIAL with type 'a monad = 'a Async.Deferred.t = struct
  type 'a monad = 'a Async.Deferred.t
  type in_channel = Async_unix.Reader.t
  type out_channel = Async_unix.Writer.t
  let pipe () =
    let inp,out = Unix.pipe () in
    let open Async_unix in
    let inp = Fd.create Fd.Kind.Fifo inp (Core.Info.of_string "mpst pipe (in)") in
    let out = Fd.create Fd.Kind.Fifo out (Core.Info.of_string "mpst pipe (out)") in
    Async_unix.Reader.create inp, Async_unix.Writer.create out
  open Async
  let input_value ch =
    Deferred.map (Async_unix.Reader.read_marshal ch) ~f:(function
    | `Ok v -> v
    | `Eof -> failwith "mpst_async: unix pipe eof")
  let input_tagged =
    input_value
  let output_value ch v =
    let raw = Marshal.to_string v Mpst.Common.marshal_flags in
    Async_unix.Writer.write ch raw;
    Deferred.return ()
  let output_tagged =
    output_value
  let flush =
    Async_unix.Writer.flushed
  let input_value_list chs =
    Deferred.List.map chs input_value
  let fork_child _ =
    (* TODO: use async-parallel?? *)
    failwith "not implemented"
end
module AsyncMonad : Mpst.S.MONAD with type 'a t = 'a Async.Deferred.t = struct
  open Async
  type 'a t = 'a Deferred.t
  let return = Deferred.return
  let return_unit = Deferred.return ()
  let bind m f = Deferred.bind m ~f
  let map f m = Deferred.map m ~f
  let iteriM f xs = Deferred.List.iteri ~f xs
  let mapM f xs = Deferred.List.map ~f xs
  let async f = Deferred.upon (f ()) ignore
end
