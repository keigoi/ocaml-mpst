let (let*) = Lwt.bind

open Bench_util.Util
open Bench_util.Testbase

module Flag = Mpst.Internal.Flag

let default_payload = snd @@ List.nth big_arrays 1

module BEventIdeal : TEST = struct
  let ch_arr = Event.new_channel ()
  let ch_unit = Event.new_channel ()

  let _ : Thread.t =
    Thread.create (fun () ->
        let rec loop () =
          ignore (Event.sync (Event.receive ch_arr));
          Event.sync (Event.send ch_unit ());
          loop ()
        in loop ()) ()

  let runtest param =
    let payload = List.assoc param big_arrays in
    Core.Staged.stage (fun () ->
        Event.sync (Event.send ch_arr payload);
        Event.sync (Event.receive ch_unit))
end

module BEventWrap : TEST = struct
  let ch_arr = Event.new_channel ()
  let ch_unit = Event.new_channel ()

  let _ : Thread.t =
    Thread.create (fun () ->
        let rec loop () =
          let `First(_) = Event.sync (Event.wrap (Event.receive ch_arr) (fun x -> `First(x))) in
          let () = Event.sync (Event.send ch_unit ()) in
          loop ()
        in loop ()) ()

  let runtest param =
    let payload = List.assoc param big_arrays in
    Core.Staged.stage (fun () ->
        let () = Event.sync (Event.send ch_arr payload) in
        let `Next(_) = Event.sync (Event.wrap (Event.receive ch_unit) (fun x -> `Next(x))) in
        ())
end

module BLwtWrap() : TEST = struct
  module M = Mpst.Internal.Stream_opt

  let ch1 = M.create ()
  let ch2 = M.create ()

  let server_step init =
    fun () ->
    let* arr_ = M.receive ch1 in
    let* () = M.send ch2 () in
    Lwt.return ()

  let client_step init =
    let payload = default_payload in
    fun () ->
    let* () = M.send ch1 payload in
    let* () = M.receive ch2 in
    Lwt.return ()

  let runtest _ =
    let server_step = server_step ch1 in
    let client_step = client_step ch2 in
    Core.Staged.stage (fun () ->
        ignore (Thread.create server_step ());
        Lwt_main.run (client_step ()))
end

module BEventCPS : TEST = struct

  let new_channel () =
    let ch = Event.new_channel () in
    (Flag.create (), ch), (Flag.create (), ch)
  let send (lin, ch) v = Flag.use lin; Event.sync (Event.send ch v)
  let receive (lin, ch) = Flag.use lin; Event.sync (Event.receive ch)
  let init_srv,init_cli = new_channel ()

  let _:Thread.t =
    Thread.create (fun () ->
        let rec loop ch =
          let arr_, `First(ch) = receive ch in
          let next,next0 = new_channel () in
          let () = send ch ((),`Next(next)) in
          loop next0
        in loop init_srv) ()

  let stored = ref init_cli
  let runtest param =
    let payload = List.assoc param big_arrays in
    Core.Staged.stage @@
      fun () ->
      let ch = !stored in
      let next,next0 = new_channel () in
      let () = send ch (payload, `First(next)) in
      let ((),`Next(ch)) = receive next0 in
      stored := ch
end

module type LWT_CHAN = sig
  type 'a t
  val create : unit -> 'a t
  val send : 'a t -> 'a -> unit Lwt.t
  val receive : 'a t -> 'a Lwt.t
end
module MpstLwtStream : LWT_CHAN = struct
  module S = Mpst.Internal.Stream_opt
  type 'a t = 'a S.t
  let create () = S.create ()
  let send t v = S.send t v
  let receive t = S.receive t
end

module BLwtIdeal(Chan:LWT_CHAN)() : TEST = struct
  type 'a seq = Seq of 'a * 'a seq lazy_t

  let ch_arr = Chan.create ()
  let ch_unit = Chan.create ()

  let server_step () =
    fun () ->
    let* arr_ = Chan.receive ch_arr in
    let* () = Chan.send ch_unit () in
    Lwt.return_unit

  let client_step () =
    let payload = default_payload in
    fun () ->
    let* () = Chan.send ch_arr payload in
    let* () = Chan.receive ch_unit in
    Lwt.return_unit

  let runtest _ =
    let server_step = server_step () in
    let client_step = client_step () in
    Core.Staged.stage (fun () ->
        ignore (Thread.create server_step ());
        Lwt_main.run (client_step ()))
end

module BLwtCPS(Chan:LWT_CHAN)() : TEST = struct

  let create () =
    let ch = Chan.create () in
    (Flag.create (), ch), (Flag.create (), ch)
  let send (lin, ch) v = Flag.use lin; Chan.send ch v
  let receive (lin, ch) = Flag.use lin; Chan.receive ch

  let server_step init =
    let stored = ref init in
    fun () ->
    let ch = !stored in
    let* `First(arr_,ch) = receive ch in
    let next,next0 = create () in
    let* () = send ch (`Next((),next)) in
    stored := next0;
    Lwt.return_unit

  let client_step param init =
    let stored = ref init in
    let payload = List.assoc param big_arrays in
    fun () ->
    let ch = !stored in
    let next,next0 = create () in
    let* () = send ch (`First(payload,next)) in
    let* `Next((),ch) = receive next0 in
    stored := ch;
    Lwt.return_unit

  let runtest =
    fun param ->
    let init_srv,init_cli = create () in
    let server_step = server_step init_srv in
    let client_step = client_step param init_cli in
    Core.Staged.stage (fun () ->
        ignore (Thread.create server_step ());
        Lwt_main.run (client_step ()))


end
