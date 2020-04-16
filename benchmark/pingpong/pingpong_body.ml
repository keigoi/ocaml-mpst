open Concur_shims
open Bench_util.Util
open Bench_util.Testbase
open Mpst.M
open Mpst.M.Base

module MakeDyn
         (Med:MEDIUM)
         ()
       : TEST
  = struct

  module Test = struct
    open Mpst
    open Mpst.Util

    let prot =
      fix (fun t ->
          (a --> b) ping @@
            (b --> a) pong @@ t)

    (**
     * pre-allocated channels
     *)
    let sa_stored, sb_stored =
      let g = gen_with_kinds [Med.medium;Med.medium;] prot in
      ref (get_ch a g), ref (get_ch b g)

    let (let*) = IO.bind

    let setup _ = ()

    let server_step _ =
      fun () ->
      let sb = !sb_stored in
      let* `ping(_,sb) = receive sb#role_A in
      let* sb = send sb#role_A#pong () in
      sb_stored := sb;
      IO.return ()

    let client_step param =
      let payload = List.assoc param big_arrays in
      Core.Staged.stage @@ fun () ->
      let sa = !sa_stored in
      let* sa = send sa#role_B#ping payload in
      let* `pong((),sa) = receive sa#role_B in
      sa_stored := sa;
      IO.return ()
  end

  include MakeTestBase(Test)(Med)()


end[@@inline]

module MakeStatic
         (Med:MEDIUM)
         ()
       : TEST
  = struct

  module Test = struct
    open Mpst_lin
    open Mpst.Util

    let prot =
      fix (fun t ->
          (a --> b) ping @@
            (b --> a) pong @@ t)

    let stored_sa, stored_sb =
      let g = raw_gen_with_kinds [Med.medium;Med.medium;] prot in
      ref (raw_get_ch a g), ref (raw_get_ch b g)

    let (let*) = Linocaml.bind

    let s = Linocaml.Zero

    let setup _ = ()

    let server_step _ =
      Linocaml.run
        (fun[@inline] () ->
        let open Linocaml in
        let open LinocamlStyle in
        let* () = put_linval s !stored_sb in
        let%lin `ping(_,#s) = s <@ receive (fun[@inline] x->x#role_A) in
        let%lin #s = s <@ send (fun[@inline] x-> x#role_A#pong) () in
        {__m=(fun[@inline] pre ->
           stored_sb := (Linocaml.lens_get s pre).__lin;
           IO.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
        )}
      )

    let client_step param =
      let payload = List.assoc param big_arrays in
      Core.Staged.stage
        (Linocaml.run
           (fun[@inline] () ->
             let open Linocaml in
             let open LinocamlStyle in
             let* () = put_linval s !stored_sa in
             let%lin #s = s <@ send (fun[@inline] x->x#role_B#ping) payload in
             let%lin `pong({Linocaml.data=()},#s) = s <@ receive (fun[@inline] x->x#role_B) in
             {__m=(fun[@inline] pre ->
                stored_sa := (Linocaml.lens_get s pre).__lin;
                IO.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
             )}
        ))

  end

  include MakeTestBase(Test)(Med)()
end[@@inline]

