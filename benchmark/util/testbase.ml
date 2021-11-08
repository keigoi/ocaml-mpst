open Util
module IO = Concur_shims.IO

let ( let* ) = IO.bind

module type TESTBED = sig
  val setup : int -> unit
  val server_step : unit -> unit -> unit IO.io
  val client_step : int -> (unit -> unit IO.io) Core.Staged.t
end

module type TEST = sig
  val runtest : int -> (unit -> unit) Core.Staged.t
end

module MakeTestBase (Test : TESTBED) (Med : MEDIUM) () : TEST = struct
  let start_server_thread step =
    let rec loop () =
      let* () = step () in
      loop ()
    in
    ignore (Thread.create loop ())

  let runtest param =
    let () = Test.setup param in
    let server_step = Test.server_step () in
    let client_step = Test.client_step param in
    if IO.is_direct then start_server_thread server_step;
    Core.Staged.stage (fun () ->
        if not IO.is_direct then
          (* run an lwt thread for every step *)
          ignore (server_step ());
        IO.main_run (Core.Staged.unstage client_step ()))
end
[@@inline]
