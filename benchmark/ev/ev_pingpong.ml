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
