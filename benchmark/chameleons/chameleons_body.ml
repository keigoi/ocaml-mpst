open Bench_util.Util
open Bench_util.Testbase
open Mpst.M
open Mpst.M.Base
module ML = Mpst_lwt.M

let () = Random.self_init ()

let from_some = function
    Some v -> v
  | None -> assert false

module MakeDyn
         (EP:Mpst.S.ENDPOINTS)
         (M:PERIPHERAL)
         (Med:MEDIUM)
         ()
       : TEST
  = struct


  module Test = struct
    type +'a monad = 'a M.t

    module Local = Local.Make(EP)(Mpst.Lin.NoCheck)(M)(M.Event)
    module Global = Global.Make(EP)(Mpst.Lin.NoCheck)(M)(M.Event)(M.Serial)
    module Util = Util.Make(EP)

    open Global
    open Local
    open Util

    let (let/) = M.bind

    let pingpong =
      (a --> b) msg @@
      (b --> a) msg @@ finish
    
    let chameleons =
      choice_at b (to_a left_or_right)
        (b, (b --> a) (left >: prot a pingpong) @@ finish)
        (b, (b --> a) (right >: prot b pingpong) @@ finish)
    
    let entrypoint =
      create_shared ~kinds:[`Local;`Local] chameleons
    
    let rec start_server n =
      if n=0 then M.return_unit else
      (* print_endline "server start."; *)
      let/ sb1 = accept entrypoint b in
      (* print_endline "found client 1."; *)
      let/ sb2 = accept entrypoint b in
      (* print_endline "found client 2."; *)
      let g = gen_with_kinds [Med.medium;Med.medium;] pingpong in
      let ta, tb = Global.get_ch a g, Global.get_ch b g in
      let/ sb1 = send sb1#role_A#left ta in
      (* print_endline "left sent."; *)
      let/ () = close sb1 in
      let/ sb2 = send sb2#role_A#right tb in
      (* print_endline "middle sent."; *)
      let/ () = close sb2 in
      start_server (n-1)
    
    let start_client i () =
      let debug str =
        (* Printf.printf "(%d): %s\n" i str;
         * flush stdout; *)
        ()
      in
      debug "connecting..";
      let/ () = M.sleep (Random.float 0.000_001) in
      let/ sa = connect entrypoint a in
      debug "connected.";
      let/ lab = receive sa#role_B in
      match lab with
      | `left(sa2,sa) -> 
         debug "left.";
         let/ sa2 = send sa2#role_B#msg () in
         let/ `msg((),sa2) = receive sa2#role_B in
         let/ () = close sa2 in
         close sa
      | `right(sb2,sa) -> 
         debug "right.";
         let/ `msg((),sb2) = receive sb2#role_A in
         let/ sb2 = send sb2#role_A#msg () in
         let/ () = close sb2 in
         close sa

    (* let ring =
     *   (a --> b) msg @@
     *   (b --> c) msg @@
     *   (c --> a) msg @@ finish
     * 
     * let chameleons =
     *   choice_at b (to_a left_or_middle_right)
     *     (b, (b --> a) (left >: prot a ring) @@ finish)
     *     (b, (choice_at b (to_a middle_or_right)
     *            (b, (b --> a) (middle >: prot b ring) @@ finish)
     *            (b, (b --> a) (right >: prot c ring) @@ finish)))
     * 
     * let entrypoint =
     *   create_shared ~kinds:[`Local;`Local] chameleons
     * 
     * let rec start_server i =
     *   if i=0 then M.return_unit else
     *   (\* print_endline "server start."; *\)
     *   let/ sb1 = accept entrypoint b in
     *   (\* print_endline "found client 1."; *\)
     *   let/ sb2 = accept entrypoint b in
     *   (\* print_endline "found client 2."; *\)
     *   let/ sb3 = accept entrypoint b in
     *   (\* print_endline "found client 3."; *\)
     *   let g = gen_with_kinds [Med.medium;Med.medium;Med.medium;] ring in
     *   let ta, tb, tc = Global.get_ch a g, Global.get_ch b g, Global.get_ch c g in
     *   let/ sb1 = send sb1#role_A#left ta in
     *   (\* print_endline "left sent."; *\)
     *   let/ () = close sb1 in
     *   let/ sb2 = send sb2#role_A#middle tb in
     *   (\* print_endline "middle sent."; *\)
     *   let/ () = close sb2 in
     *   let/ sb3 = send sb3#role_A#right tc in
     *   (\* print_endline "right sent."; *\)
     *   let/ () = close sb3 in
     *   start_server (i-1)
     * 
     * 
     * let start_client i () =
     *   let debug str =
     *     (\* Printf.printf "(%d): %s\n" i str;
     *      * flush stdout; *\)
     *     ()
     *   in
     *   debug "connecting..";
     *   let/ sa = connect entrypoint a in
     *   debug "connected.";
     *   let/ lab = receive sa#role_B in
     *   match lab with
     *   | `left(sa2,sa) -> 
     *      debug "left.";
     *      let/ sa2 = send sa2#role_B#msg () in
     *      let/ `msg((),sa2) = receive sa2#role_C in
     *      let/ () = close sa2 in
     *      close sa
     *   | `middle(sb2,sa) -> 
     *      debug "middle.";
     *      let/ `msg((),sb2) = receive sb2#role_A in
     *      let/ sb2 = send sb2#role_C#msg () in
     *      let/ () = close sb2 in
     *      close sa
     *   | `right(sc2,sa) -> 
     *      debug "right.";
     *      let/ `msg((),sc2) = receive sc2#role_B in
     *      let/ sc2 = send sc2#role_A#msg () in
     *      let/ () = close sc2 in
     *      close sa *)

    let rec loop f x = M.bind (f x) (fun () -> loop f x)

    let setup n =
      let _ : unit list =
        List.init n begin fun i ->
          M.async
            (fun () ->
              (* Printf.printf "thread %d started\n" i; *)
              loop (start_client i) ())
          end
      in
      ()

    let (let/) = M.bind

    let server_step _ _ =
      M.return_unit

    let client_step n =
      Core.Staged.stage @@
        fun () ->
      (* print_endline "setup"; *)
        start_server (n/2)
        (* print_endline "bench thread"; *)
  end

  include MakeTestBase(Test)(M)(Med)()


end(* [@@inline] *)

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
      sa_stored := Some (Global.raw_get_ch a g);
      sb_stored := Some (Global.raw_get_ch b g);
      ()

    let (let/) = M.Linocaml.(>>=)

    let s = Linocaml.Zero

    let server_step _ =
      let sb_stored = ref (!sb_stored) in
      M.Linocaml.run'
      (fun[@inline] () ->
        let open M.Linocaml in
        let/ () = put_linval s (from_some !sb_stored) in
        let%lin `ping(_,#s) = s <@ receive (fun[@inline] x->x#role_A) in
        let%lin #s = s <@ send (fun[@inline] x-> x#role_A#pong) () in
        {__m=(fun[@inline] pre ->
           sb_stored := Some ((Linocaml.lens_get s pre).__lin);
           M.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
        )}
      )

    let client_step param =
      let payload = () in
      Core.Staged.stage
        (M.Linocaml.run'
           (fun[@inline] () ->
             let open M.Linocaml in
             let/ () = put_linval s @@ from_some !sa_stored in
             let%lin #s = s <@ send (fun[@inline] x->x#role_B#ping) payload in
             let%lin `pong({Linocaml.data=()},#s) = s <@ receive (fun[@inline] x->x#role_B) in
             {__m=(fun[@inline] pre ->
                sa_stored := Some (Linocaml.lens_get s pre).__lin;
                M.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
             )}
        ))

  end

  include MakeTestBase(Test)(M)(Med)()
end[@@inline]
