open Util
open Mpst.M
open Mpst.M.Base

let default_buffer_size = Lwt_io.default_buffer_size ()

module type TESTBED = sig
  type +'a monad
  val setup : int -> unit
  val server_step : int -> unit -> unit monad
  val client_step : int -> (unit -> unit monad) Core.Staged.t
end
module type TEST = sig
  val runtest : int -> (unit -> unit) Core.Staged.t
end

module MakeTestBase
         (Test:TESTBED)
         (M:PERIPHERAL with type 'a t = 'a Test.monad)
         (Med:MEDIUM)
         ()
       : TEST
  = struct

  let loop f =
    fun cnt ->
    let rec loop cnt =
      if cnt = Some 0 then begin
          M.return ()
        end else begin
          M.bind (f ()) @@ fun () ->
          (loop[@tailcall]) (Mpst.M.Common.map_option (fun x->x-1) cnt)
        end
    in
    loop cnt

  let runtest_repeat ~count ~param =
    Test.setup param;
    if Med.medium = `IPCProcess then begin
        (* ignore (M.Serial.fork_child (fun () -> M.run (loop (Test.server_step param) None))); *)
      end else if M.is_direct then begin
        thread (fun () -> M.run (loop (Test.server_step param) None)) ()
      end else begin
        (**)
      end;
    let server_step = Test.server_step param in
    Core.Staged.stage
      (fun () ->
        if Med.medium = `IPCProcess then begin
            (**)
          end else if M.is_direct then begin
            (**)
          end else begin
            M.async (fun () -> loop server_step (Some count))
          end;
        M.run (loop (Core.Staged.unstage (Test.client_step param)) (Some count))
      )

  let runtest param = runtest_repeat ~count:1 ~param

  (* (\* start server thread *\)
   * let () =
   *   if Med.medium = `IPCProcess then begin
   *       (\* ignore (M.Serial.fork_child (fun () -> M.run (loop Test.server_step None))); *\)
   *     end else if M.is_direct then begin
   *       (\* thread (fun () -> M.run (loop Test.server_step None)) (); *\)
   *     end *)
end[@@inline]
