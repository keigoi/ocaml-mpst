open Core_bench.Bench
open Core_bench.Bench.Test
open Bench_util
open Bench_util.Util
open Bench_util.Testbase
module Flag = Mpst.Internal.Flag

let default_payload = snd @@ List.nth big_arrays 1

module BEventIdeal : TEST = struct
  let ch_arr = Event.new_channel ()
  let ch_unit = Event.new_channel ()

  let (_ : Thread.t) =
    Thread.create
      (fun () ->
        let rec loop () =
          ignore (Event.sync (Event.receive ch_arr));
          Event.sync (Event.send ch_unit ());
          loop ()
        in
        loop ())
      ()

  let runtest param =
    let payload = List.assoc param big_arrays in
    Core.Staged.stage (fun () ->
        Event.sync (Event.send ch_arr payload);
        Event.sync (Event.receive ch_unit))
end

module BEventCPS : TEST = struct
  let new_channel () =
    let ch = Event.new_channel () in
    ((Flag.create (), ch), (Flag.create (), ch))

  let send (lin, ch) v =
    Flag.use lin;
    Event.sync (Event.send ch v)

  let receive (lin, ch) =
    Flag.use lin;
    Event.sync (Event.receive ch)

  let init_srv, init_cli = new_channel ()

  let (_ : Thread.t) =
    Thread.create
      (fun () ->
        let rec loop ch =
          let arr_, `First ch = receive ch in
          let next, next0 = new_channel () in
          let () = send ch ((), `Next next) in
          loop next0
        in
        loop init_srv)
      ()

  let stored = ref init_cli

  let runtest param =
    let payload = List.assoc param big_arrays in
    Core.Staged.stage @@ fun () ->
    let ch = !stored in
    let next, next0 = new_channel () in
    let () = send ch (payload, `First next) in
    let (), `Next ch = receive next0 in
    stored := ch
end

module Make_IPC () : TEST = struct
  let ( let* ) = IO.bind

  type pipe = { inp : IO.in_channel; out : IO.out_channel }
  type t = { me : pipe; othr : pipe }

  let new_dpipe () =
    let my_inp, otr_out = IO.pipe () in
    let otr_inp, my_out = IO.pipe () in
    {
      me = { inp = my_inp; out = my_out };
      othr = { inp = otr_inp; out = otr_out };
    }

  let flip { me = othr; othr = me } = { me; othr }

  let fork_child f =
    let pid = Unix.fork () in
    if pid = 0 then (
      (f () : unit);
      exit 0)
    else pid

  let ch = new_dpipe ()

  let () =
    ignore
    @@ fork_child (fun () ->
           let ch = flip ch in
           let rec loop () =
             let _ = input_value ch.me.inp in
             let _ = output_value ch.me.out () in
             let () = flush ch.me.out in
             loop ()
           in
           loop ())

  let runtest param =
    let payload = List.assoc param big_arrays in
    Core.Staged.stage @@ fun () ->
    let () = output_value ch.me.out payload in
    let () = flush ch.me.out in
    let () = input_value ch.me.inp in
    ()
end

(* array size parameters for ipc payloads *)
let args = array_sizes
let run f = Core.Staged.unstage (f (List.nth array_sizes 0))

let test_bare_pingpong =
  if IO.is_direct then
    (* ev only *)
    [
      create ~name:"ev_bare" (run BEventIdeal.runtest);
      create ~name:"ev_bare_cps" (run BEventCPS.runtest);
      create_indexed ~args ~name:"ipc_bare"
        (let module M = Make_IPC () in
        M.runtest);
    ]
  else []

let () = Core.Command.run @@ Core_bench.Bench.make_command test_bare_pingpong
