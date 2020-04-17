open Util

module IO = Concur_shims.IO

module type TESTBED = sig
  val setup : int -> unit
  val server_step : unit -> unit -> unit IO.io
  val client_step : int -> (unit -> unit IO.io) Core.Staged.t
end
module type TEST = sig
  val runtest : int -> (unit -> unit) Core.Staged.t
end

module MakeTestBase
         (Test:TESTBED)
         (Med:MEDIUM)
         ()
       : TEST
     = struct

    let runtest param =
      let () = Test.setup param in
      let server_step = Test.server_step () in
      let client_step = Test.client_step param in
      Core.Staged.stage
        (fun () ->
           if not IO.is_direct then begin
             (* run a lwt thread for every step *)
             IO.async (fun () -> server_step ());
           end;
           IO.main_run (Core.Staged.unstage client_step ());
        )
  end[@@inline]
