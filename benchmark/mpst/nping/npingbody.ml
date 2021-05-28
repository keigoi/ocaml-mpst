open Concur_shims
open Bench_util.Util
open Bench_util.Testbase
open Mpst
open Mpst.Util
   
let from_some = function
    Some v -> v
  | None -> assert false

module MakeDyn
         (Med:MEDIUM)
         ()
       : TEST
  = struct


  module Test = struct

    let rec gen_nping n t =
      if n = 0 then
        t
      else
        (a --> b) ping @@
        (b --> a) pong @@ gen_nping (n-1) t

    let nping n =
      fix (fun t -> gen_nping n t)
      
    (**
     * pre-allocated channels
     *)
    let sa_init, sb_init =
      ref None, ref None

    let setup n =
      let g = gen_with_kinds [Med.medium;Med.medium;] (nping n)  in
      sa_init := Some (get_ch a g);
      sb_init := Some (get_ch b g);
      ()

    let (let*) = IO.bind

    let server_step _ =
      let sb_stored = ref (!sb_init) in
      fun () ->
      let sb = from_some !sb_stored in
      (* print_endline "server step (start)"; *)
      let* `ping(_,sb) = receive sb#role_A in
      (* print_endline "server step (received)"; *)
      let* sb = send sb#role_A#pong () in
      (* print_endline "server step (sent)"; *)
      sb_stored := Some sb;
      IO.return ()

    let client_step _ =
      let sa_stored = ref (!sa_init) in
      let payload = () in
      Core.Staged.stage @@ fun () ->
      (* print_endline "client step (start)"; *)
      let sa = from_some !sa_stored in
      let* sa = send sa#role_B#ping payload in
      (* print_endline "client step (sent)"; *)
      let* `pong((),sa) = receive sa#role_B in
      (* print_endline "client step (received)"; *)
      sa_stored := Some sa;
      IO.return ()
  end

  include MakeTestBase(Test)(Med)()


end[@@inline]

module MakeStatic
         (Med:MEDIUM)
         ()
       (* : TEST *)
  = struct
  open Mpst_lin
  module Test = struct
    let rec gen_nping n t =
      if n = 0 then
        t
      else
        (a --> b) ping @@
        (b --> a) pong @@ gen_nping (n-1) t

    let nping n =
      fix (fun t -> gen_nping n t)
      
    (**
     * pre-allocated channels
     *)
    let sa_stored, sb_stored =
      ref None, ref None

    let setup n =
      let g = raw_gen_with_kinds [Med.medium;Med.medium;] (nping n)  in
      sa_stored := Some (raw_get_ch a g);
      sb_stored := Some (raw_get_ch b g);
      ()

    let s = Linocaml.Zero

    open Linocaml

    let server_step _ =
      let rsb = ref (from_some !sb_stored) in
      (fun[@inline] () ->
        let sb = !rsb in
        let* (_:all_empty), {__lin=sb} =
          Syntax.Internal._run
            (Syntax.Internal._modify (Syntax.lens_put' s sb) @@
               let%lin `ping(_,#s) = receive s (fun[@inline] x->x#role_A) in
               send s (fun[@inline] x-> x#role_A#pong) ()
            ) Syntax.Internal._all_empty
        in
        rsb := sb;
        Lwt.return_unit
      )

    let (let/) = Linocaml.bind

    let client_step param =
      let payload = () in
      Core.Staged.stage
        (fun[@inline] () ->
          let sa = from_some !sa_stored in
          let* (_:all_empty), {__lin=`pong({Linocaml.data=()},{__lin=sa})} =
          Syntax.Internal._run
            (Syntax.Internal._modify (Syntax.lens_put' s sa) @@
             let%lin #s = send s (fun[@inline] x->x#role_B#ping) payload in
             receive s (fun[@inline] x->x#role_B)
            ) Syntax.Internal._all_empty
          in
          sa_stored := Some sa;
          Lwt.return_unit
        )
  end

  include MakeTestBase(Test)(Med)()
end[@@inline]
