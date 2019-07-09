open Util

open Mpst.M
open Mpst.M.Base
module ML = Mpst_lwt.M

module type TESTBED = sig
  type +'a monad 
  val server_step : unit -> unit monad
  val client_step : int -> (unit -> unit monad) Core.Staged.t
end
module type TEST = sig
  val runtest_repeat : count:int -> param:int -> (unit -> unit) Core.Staged.t
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
        fork (fun () -> M.run (loop Test.server_step None)) ();
      end else if M.is_direct then begin
        thread (fun () -> M.run (loop Test.server_step None)) ();
      end
end

module MakeDyn
         (D:DYNCHECK)
         (M:PERIPHERAL)
         (Med:MEDIUM)
         ()
       : TEST
  = struct


  module Test = struct
    type +'a monad = 'a M.t

    module Local = Local.Make(D.EP)(D.Flag)(M)(M.Event)
    module Global = Global.Make(D.EP)(M)(M.Event)(M.Serial)(NoLin)
    module Util = Util.Make(D.EP)

    open Global
    open Local
    open Util

    let prot =
      fix (fun t ->
          (a --> b) ping @@
            (b --> a) pong @@ t)

    (** 
     * pre-allocated channels 
     *)
    let sa_stored, sb_stored =
      let g = gen_with_kinds [Med.medium;Med.medium;] prot in
      ref (Global.get_ep a g), ref (Global.get_ep b g)

    let (let/) = M.bind

    let server_step =
      fun () ->
      let sb = !sb_stored in
      let/ `ping(_,sb) = receive sb#role_A in
      let/ sb = send sb#role_A#pong () in
      sb_stored := sb;
      M.return_unit

    let client_step _ =
      Core.Staged.stage @@ fun () ->
      let sa = !sa_stored in
      let/ sa = send sa#role_B#ping default_payload in
      let/ `pong((),sa) = receive sa#role_B in
      sa_stored := sa;
      M.return_unit
  end

  include MakeTestBase(Test)(M)(Med)()


end

module MakeStatic
         (M:PERIPHERAL_LIN)
         (Med:MEDIUM)
         ()
       : TEST
  = struct

  module Test = struct
    module Local = Mpst_monad.Local_monad.Make(M)(M.Event)(M.Linocaml)
    module Global = Mpst_monad.Global_monad.Make(M)(M.Event)(M.Serial)(M.Linocaml)
    module Util = Util.Make(Mpst.M.Nocheck.Nodyncheck)
    open Global
    open Local
    open Util

    type +'a monad = 'a M.t

    let prot =
      fix (fun t ->
          (a --> b) ping @@
            (b --> a) pong @@ t)

    let stored_sa, stored_sb =
      let g = raw_gen_with_kinds [Med.medium;Med.medium;] prot in
      ref (Global.raw_get_ep a g), ref (Global.raw_get_ep b g)

    let (let/) = M.Linocaml.(>>=)

    let s = Linocaml.Zero

    let server_step =
      M.Linocaml.run'
      (fun () ->
        let open M.Linocaml in
        let/ () = put_linval s !stored_sb in
        let%lin `ping(_,#s) = s <@ receive (fun x->x#role_A) in
        let%lin #s = s <@ send (fun x-> x#role_A#pong) () in
        {__m=(fun pre ->
           stored_sb := (Linocaml.lens_get s pre).__lin;
           M.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
        )}
      )

    let client_step _ =
      Core.Staged.stage
        (M.Linocaml.run'
           (fun () ->
             let open M.Linocaml in
             let/ () = put_linval s !stored_sa in
             let%lin #s = s <@ send (fun x->x#role_B#ping) default_payload in
             let%lin `pong({Linocaml.data=()},#s) = s <@ receive (fun x->x#role_B) in
             {__m=(fun pre ->
                stored_sa := (Linocaml.lens_get s pre).__lin;
                M.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
             )}
        ))

  end

  include MakeTestBase(Test)(M)(Med)()
end

module BRefImpl : TEST
  = struct

  module Test = struct
    type +'a monad = 'a
    open Mpst_ref

    let prot =
      fix (fun t ->
          (a --> b) ping @@
            (b --> a) pong @@ t)

    let stored_sa, stored_sb =
      let g = gen prot in
      ref (Global.get_ep a g), ref (Global.get_ep b g)

    let (let/) m f = f m

    let server_step () =
      let sb = !stored_sb in
      let/ `ping(_,sb) = receive sb#role_A in
      let/ sb = send sb#role_A#pong () in
      stored_sb := sb

    let client_step _param =
      Core.Staged.stage (fun () ->
          let sa = !stored_sa in
          let/ sa = send sa#role_B#ping default_payload in
          let/ `pong((), sa) = receive sa#role_B in
          stored_sa := sa
        )
  end

  include MakeTestBase(Test)(Direct)(Shmem)()
end
