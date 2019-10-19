open Util
open Testbase
open Mpst.M
open Mpst.M.Base
module ML = Mpst_lwt.M

module MakeDyn
         (EP:Mpst.S.ENDPOINTS)
         (M:PERIPHERAL)
         (Med:MEDIUM)
         ()
       : TEST
  = struct


  module Test = struct
    type +'a monad = 'a M.t

    (* module EP = Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheckClosure(Mpst.LinFlag.PosixMutexFlag)) *)
    (* module EP = Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheck(Dyncheck_nanomutex.NanoMutexFlag)) *)
    (* module EP = Mpst.Endpoints.Make(Mpst.Lin.MakeDynCheckClosure(Dyncheck_nanomutex.NanoMutexFlag)) *)
    (* module EP = Mpst.Endpoints.Make(Mpst.Lin.NoCheck) *)

    module Local = Local.Make(EP)(Mpst.Lin.NoCheck)(M)(M.Event)
    module Global = Global.Make(EP)(Mpst.Lin.NoCheck)(M)(M.Event)(M.Serial)
    module Util = Util.Make(EP)

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
      ref (Global.get_ch a g), ref (Global.get_ch b g)

    let (let/) = M.bind

    let server_step =
      fun () ->
      let sb = !sb_stored in
      let/ `ping(_,sb) = receive sb#role_A in
      let/ sb = send sb#role_A#pong () in
      sb_stored := sb;
      M.return_unit

    let client_step param =
      let payload = List.assoc param big_arrays in
      Core.Staged.stage @@ fun () ->
      let sa = !sa_stored in
      let/ sa = send sa#role_B#ping payload in
      let/ `pong((),sa) = receive sa#role_B in
      sa_stored := sa;
      M.return_unit
  end

  include MakeTestBase(Test)(M)(Med)()


end[@@inline]

module MakeStatic
         (M:PERIPHERAL_LIN)
         (Med:MEDIUM)
         ()
       : TEST
  = struct

  module Test = struct
    module Local = Mpst_monad.Local_monad.Make(M)(M.Event)(M.Linocaml)
    module Global = Mpst_monad.Global_monad.Make(M)(M.Event)(M.Serial)(M.Linocaml)
    module Util = Util.Make(Mpst_monad.Linocaml_lin.EP)
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
      ref (Global.raw_get_ch a g), ref (Global.raw_get_ch b g)

    let (let/) = M.Linocaml.(>>=)

    let s = Linocaml.Zero

    let server_step =
      M.Linocaml.run'
      (fun[@inline] () ->
        let open M.Linocaml in
        let/ () = put_linval s !stored_sb in
        let%lin `ping(_,#s) = s <@ receive (fun[@inline] x->x#role_A) in
        let%lin #s = s <@ send (fun[@inline] x-> x#role_A#pong) () in
        {__m=(fun[@inline] pre ->
           stored_sb := (Linocaml.lens_get s pre).__lin;
           M.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
        )}
      )

    let client_step param =
      let payload = List.assoc param big_arrays in
      Core.Staged.stage
        (M.Linocaml.run'
           (fun[@inline] () ->
             let open M.Linocaml in
             let/ () = put_linval s !stored_sa in
             let%lin #s = s <@ send (fun[@inline] x->x#role_B#ping) payload in
             let%lin `pong({Linocaml.data=()},#s) = s <@ receive (fun[@inline] x->x#role_B) in
             {__m=(fun[@inline] pre ->
                stored_sa := (Linocaml.lens_get s pre).__lin;
                M.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
             )}
        ))

  end

  include MakeTestBase(Test)(M)(Med)()
end[@@inline]

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
      ref (Global.get_ch a g), ref (Global.get_ch b g)

    let (let/) m f = f m

    let server_step () =
      let sb = !stored_sb in
      let/ `ping(_,sb) = receive sb#role_A in
      let/ sb = send sb#role_A#pong () in
      stored_sb := sb

    let client_step param =
      let payload = List.assoc param big_arrays in
      Core.Staged.stage (fun () ->
          let sa = !stored_sa in
          let/ sa = send sa#role_B#ping payload in
          let/ `pong((), sa) = receive sa#role_B in
          stored_sa := sa
        )
  end

  include MakeTestBase(Test)(Direct)(Shmem)()
end
