open Concur_shims
open Util

(* let default_buffer_size = Lwt_io.default_buffer_size () *)

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

  let rec forever f () =
    IO.bind (f ()) @@ fun () ->
    (forever[@tailcall]) f ()

  let start_server_threads () =
    if Med.medium = `IPCProcess then begin
        (* ignore (M.Serial.fork_child (fun () -> M.run (forever (Test.server_step ()) ()))); *)
      end else if IO.is_direct then begin
        thread (fun () -> IO.main_run (forever (Test.server_step ()) ())) ()
      end

  let runtest_repeat ~param =
    Test.setup param;
    start_server_threads ();
    let server_step = Test.server_step () in
    Core.Staged.stage
      (fun () ->
        if Med.medium <> `IPCProcess && not IO.is_direct then begin
            ignore (Thread.create (fun () -> server_step ()) ())
          end;
        IO.main_run (Core.Staged.unstage (Test.client_step param) ())
      )

  let runtest param = runtest_repeat ~param


  (* start server threads *)
  (* let () = start_server_threads () *)
end[@@inline]
