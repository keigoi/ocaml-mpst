open Concur_shims
open Bench_util.Util
open Bench_util.Testbase

let global_cnt = ref 0

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
    let sa_init, sb_init =
      let g = gen_with_kinds [Med.medium;Med.medium;] prot in
      ref (get_ch a g), ref (get_ch b g)


    let setup _ =
      let g = gen_with_kinds [Med.medium;Med.medium;] prot in
      sa_init := get_ch a g;
      sb_init := get_ch b g

    let server_step _ =
      let stored = ref !sb_init in
      fun () ->
      let sb = !stored in
      IO.bind (receive sb#role_A) (fun[@inline] (`ping(_,sb)) ->
      IO.bind (send sb#role_A#pong ()) (fun[@inline] sb ->
      stored := sb;
      IO.return_unit))

    let client_step param =
      let stored = ref !sa_init in
      let payload = List.assoc param big_arrays in
      Core.Staged.stage @@ fun () ->
      let sa = !stored in
      IO.bind (send sa#role_B#ping payload) (fun[@Inline] sa ->
      IO.bind (receive sa#role_B) (fun[@inline] (`pong(c,sa)) ->
      stored := sa;
      IO.return_unit))
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
    open Linocaml
    open LinocamlStyle

    let prot =
      fix (fun t ->
          (a --> b) ping @@
            (b --> a) pong @@ t)

    let sa_init, sb_init =
      let g = raw_gen_with_kinds [Med.medium;Med.medium;] prot in
      ref (raw_get_ch a g), ref (raw_get_ch b g)

    let (let*) = Linocaml.bind

    let s = Linocaml.Zero

    let setup _ =
      let g = raw_gen_with_kinds [Med.medium;Med.medium;] prot in
      sa_init := raw_get_ch a g;
      sb_init := raw_get_ch b g


    let server_step _ =
      let store = ref !sb_init in
      Linocaml.run
        (fun[@inline] () ->
        Linocaml.bind (put_linval s !store) (fun[@inline] () ->
        let%lin `ping(_,#s) = s <@ receive (fun[@inline] x->x#role_A) in
        let%lin #s = s <@ send (fun[@inline] x-> x#role_A#pong) () in
        {__m=(fun[@inline] pre ->
           store := (Linocaml.lens_get s pre).__lin;
           IO.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
        )}
      ))

    let client_step param =
      let store = ref !sa_init in
      let payload = List.assoc param big_arrays in
      Core.Staged.stage
        (Linocaml.run
           (fun[@inline] () ->
             Linocaml.bind (put_linval s !store) (fun[@inline] () ->
             let%lin #s = s <@ send (fun[@inline] x->x#role_B#ping) payload in
             let%lin `pong({Linocaml.data=()},#s) = s <@ receive (fun[@inline] x->x#role_B) in
             {__m=(fun[@inline] pre ->
                store := (Linocaml.lens_get s pre).__lin;
                IO.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
             )}
        )))

  end

  include MakeTestBase(Test)(Med)()
end[@@inline]
