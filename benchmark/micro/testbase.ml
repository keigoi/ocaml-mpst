open Util
open Mpst.M
open Mpst.M.Base

module type TESTBED = sig
  type +'a monad 
  val server_step : unit -> unit monad
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
      if cnt = Some 0 then
        M.return ()
      else
        M.bind (f ()) @@ fun () ->
        (loop[@tailcall]) (Mpst.M.Common.map_option (fun x->x-1) cnt)
    in
    loop cnt

  let runtest_repeat ~count ~param =
    Core.Staged.stage
      (fun () ->
        if Med.medium = `IPCProcess then begin
            (* counterpart runs in another proess *)
          end else if M.is_direct then begin
            (* counterpart runs in another thread *)
          end else begin
            (* run here -- lwt thread seems to be better run in same Lwt_main.run *)
            M.async (fun () -> loop Test.server_step (Some count))
          end;
        M.run (loop (Core.Staged.unstage (Test.client_step param)) (Some count))
      )

  let runtest param = runtest_repeat ~count:1 ~param

  (* start server thread *)
  let () =
    if Med.medium = `IPCProcess then begin
        ignore (M.Serial.fork_child (fun () -> M.run (loop Test.server_step None)));
      end else if M.is_direct then begin
        thread (fun () -> M.run (loop Test.server_step None)) ();
      end
end
