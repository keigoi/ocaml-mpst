open Core_bench.Bench
open Core_bench.Bench.Test
open Bench_util
open Bench_util.Util
let (let*) = Lwt.bind

open Bench_util.Util
open Bench_util.Testbase

module Flag = Mpst.Internal.Flag

let default_payload = snd @@ List.nth big_arrays 1

module type LWT_CHAN = sig
  type 'a t
  val create : unit -> 'a t
  val send : 'a t -> 'a -> unit Lwt.t
  val receive : 'a t -> 'a Lwt.t
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
        ignore (server_step ());
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
        ignore (server_step ());
        Lwt_main.run (client_step ()))
end

module MpstStream : LWT_CHAN = struct
  module S = Mpst.Internal.Stream_opt
  type 'a t = 'a S.t
  let create () = S.create ()
  let send t v = S.send t v
  let receive t = S.receive t
end


(* array size parameters for ipc payloads *)
let args = array_sizes

let run f = Core.Staged.unstage (f (List.nth array_sizes 0))

let test_bare_pingpong =
  if IO.is_direct then
    []
  else
    [
      create ~name:"lwt_bare" (let module M = BLwtIdeal(MpstStream)() in run M.runtest);
      create ~name:"lwt_bare_cps" (let module M = BLwtCPS(MpstStream)() in run M.runtest);
    ]

let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      test_bare_pingpong
