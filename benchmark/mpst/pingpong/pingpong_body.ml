open Concur_shims
open Bench_util.Util
open Bench_util.Testbase
open Linocaml

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
      IO.bind (send sa#role_B#ping payload) (fun[@inline] sa ->
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

    let prot =
      fix (fun t ->
          (a --> b) ping @@
            (b --> a) pong @@ t)

    let sa_init, sb_init =
      let g = raw_gen_with_kinds [Med.medium;Med.medium;] prot in
      ref (raw_get_ch a g), ref (raw_get_ch b g)

    let s = Linocaml.Zero

    let setup _ =
      let g = raw_gen_with_kinds [Med.medium;Med.medium;] prot in
      sa_init := raw_get_ch a g;
      sb_init := raw_get_ch b g


    let server_step _ =
      let store = ref !sb_init in
      fun _ ->
      let* (_:all_empty), {__lin=sa} =
        Syntax.Internal._run
          (Syntax.Internal._modify (Syntax.lens_put' s !store) @@
             let%lin `ping(_,#s) = receive s (fun[@inline] x->x#role_A) in
             send s (fun[@inline] x-> x#role_A#pong) ()
          ) Syntax.Internal._all_empty
      in
      store := sa;
      Lwt.return_unit

    let client_step param =
      let store = ref !sa_init in
      let payload = List.assoc param big_arrays in
      Core.Staged.stage
        (fun _ ->
          let* (_:all_empty), {__lin=`pong({Linocaml.data=()},{__lin=sb})} =
            Syntax.Internal._run
             (Syntax.Internal._modify (Syntax.lens_put' s !store) @@
               let%lin #s = send s (fun[@inline] x->x#role_B#ping) payload in
               receive s (fun[@inline] x->x#role_B)
             ) Syntax.Internal._all_empty
          in
          store := sb;
          Lwt.return_unit;
        )
  end

  include MakeTestBase(Test)(Med)()
end[@@inline]
