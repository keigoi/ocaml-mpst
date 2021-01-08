open Util

module IO = Concur_shims.IO
let (let*) = IO.bind

module type TESTBED = sig
  val setup : int -> unit
  val server_step : int -> unit -> unit IO.io
  val client_step : int -> (unit -> unit IO.io) Core.Staged.t
end
module type TEST = sig
  val runtest : int -> (unit -> unit) Core.Staged.t
end

module MakeTestBase
         (Test:TESTBED)
         (Med:MEDIUM)
         ()
       : TEST = struct

  let start_server_thread step =
    let rec loop () =
      let* () = step () in
      loop ()
    in
    ignore (Thread.create loop ())

  let runtest param =
    let () = Test.setup param in
    let server_step = Test.server_step param in
    let client_step = Test.client_step param in
    if IO.is_direct then begin
        start_server_thread server_step
      end;
    Core.Staged.stage
      (fun () ->
        if not IO.is_direct then begin
            (* run an lwt thread for every step *)
            ignore (server_step ());
          end;
        IO.main_run (Core.Staged.unstage client_step ());
      )
    
end[@@inline]
