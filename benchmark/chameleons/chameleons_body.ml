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
      let/ () = M.yield () in
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

    let rec loop f x = M.bind (f x) (fun () -> loop f x)

    let setup n =
      (* if Med.medium = `IPCProcess || M.is_direct then begin *)
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
        fun () -> start_server (n/2)
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

    let pingpong =
      (a --> b) msg @@
      (b --> a) msg @@ finish

    let chameleons =
      choice_at b (to_a left_or_right)
        (b, (b --> a) (left >: prot a pingpong) @@ finish)
        (b, (b --> a) (right >: prot b pingpong) @@ finish)

    let entrypoint =
      create_shared ~kinds:[`Local;`Local] chameleons

      let sa,sa1,sa2, ta,tb, g =
        let open Linocaml in
        let sa = Zero in
        let sa1 = Succ sa in
        let sa2 = Succ sa1 in
        let ta = Succ sa2 in
        let tb = Succ ta in
        let g = Succ tb in
        sa,sa1,sa2,ta,tb,g
      let sb1 = sa1
      let sb2 = sa2

    let rec start_server n =
      let open M.Linocaml in
      if n=0 then return () else begin
        let%lin #sb1 = accept entrypoint b in
        let%lin #sb2 = accept entrypoint b in
        let%lin #g = Global.gen_with_kinds [Med.medium;Med.medium;] pingpong in
        let%lin #g,#ta = g <@ Global.get_ch a in
        let%lin #g,#tb = g <@ Global.get_ch b in
        g <@ Global.degen >>
        let%lin #sb1 = (sb1@*ta) <@ deleg_send (fun x -> x#role_A#left) in
        sb1 <@ close >>
        let%lin #sb2 = (sb2@*tb) <@ deleg_send (fun x -> x#role_A#right) in
        sb2 <@ close >>
        start_server (n-1)
      end


    let start_client i () =
      let open M.Linocaml in
      let debug str =
        (* Printf.printf "(%d): %s\n" i str;
         * flush stdout; *)
        ()
      in
      debug "connecting..";
      lift M.yield () >>
      let%lin #sa = connect entrypoint a in
      debug "connected.";
      match%lin sa <@ receive (fun x->x#role_B) with
      | `left(#sa2,#sa) ->
         debug "left.";
         let%lin #sa2 = sa2 <@ send (fun x->x#role_B#msg) () in
         let%lin `msg(_,#sa2) = sa2 <@ receive (fun x ->x#role_B) in
         (sa2 <@ close) >>
         (sa <@ close)
      | `right(#sb2,#sa) ->
         debug "right.";
         let%lin `msg(_,#sb2) = sb2 <@ receive (fun x->x#role_A) in
         let%lin #sb2 = sb2 <@ send (fun x ->x#role_A#msg) () in
         (sb2 <@ close) >>
         (sa <@ close)

    let rec loop f x = M.bind (f x) (fun () -> loop f x)

    let setup n =
      (* if Med.medium = `IPCProcess || M.is_direct then begin *)
      let _ : unit list =
        List.init n begin fun i ->
          M.async
            (fun () ->
              (* Printf.printf "thread %d started\n" i; *)
              loop (M.Linocaml.run' (start_client i)) ())
          end
      in
      ()


    let server_step _ _ =
      M.return_unit

    let client_step n =
      Core.Staged.stage @@
        fun () -> M.Linocaml.run' (fun () -> start_server (n/2)) ()
  end

  include MakeTestBase(Test)(M)(Med)()
end[@@inline]