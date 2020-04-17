open Concur_shims
open Bench_util.Util
open Bench_util.Testbase

let () = Random.self_init ()

let from_some = function
    Some v -> v
  | None -> assert false

module MakeDyn
         (Med:MEDIUM)
         ()
       : TEST
  = struct


  module Test = struct
    open Mpst
    open Mpst.Util

    let (let*) = IO.bind

    let pingpong =
      (a --> b) msg @@
      (b --> a) msg @@ finish

    let chameleons =
      choice_at b (to_a left_or_right)
        (b, (b --> a) (left >: get_ty a pingpong) @@ finish)
        (b, (b --> a) (right >: get_ty b pingpong) @@ finish)

    let entrypoint =
      create_shared ~kinds:[`Local,0;`Local,0] chameleons

    let rec start_server n =
      if n=0 then IO.return () else begin
        (* print_endline "server start."; *)
        let* sb1 = accept entrypoint b in
        (* print_endline "found client 1."; *)
        let* sb2 = accept entrypoint b in
        (* print_endline "found client 2."; *)
        let g = gen_with_kinds [Med.medium;Med.medium;] pingpong in
        let ta, tb = get_ch a g, get_ch b g in
        let* sb1 = send sb1#role_A#left ta in
        (* print_endline "left sent."; *)
        let* () = close sb1 in
        let* sb2 = send sb2#role_A#right tb in
        (* print_endline "middle sent."; *)
        let* () = close sb2 in
        start_server (n-1)
      end


    let start_client i () =
      let debug str =
        (* Printf.printf "(%d): %s\n" i str;
         * flush stdout; *)
        ()
      in
      debug "connecting..";
      let* () = IO.yield () in
      let* sa = connect entrypoint a in
      debug "connected.";
      let* lab = receive sa#role_B in
      match lab with
      | `left(sa2,sa) ->
         debug "left.";
         let* sa2 = send sa2#role_B#msg () in
         let* `msg((),sa2) = receive sa2#role_B in
         let* () = close sa2 in
         close sa
      | `right(sb2,sa) ->
         debug "right.";
         let* `msg((),sb2) = receive sb2#role_A in
         let* sb2 = send sb2#role_A#msg () in
         let* () = close sb2 in
         close sa

    let rec loop f x = IO.bind (f x) (fun () -> loop f x)

    let setup n =
      let _ : unit list =
        List.init n begin fun i ->
          ignore (Thread.create 
            (fun () ->
              (* Printf.printf "thread %d started\n" i; *)
              loop (start_client i) ()) ())
          end
      in
      ()

    let server_step _ _ =
      IO.return_unit

    let client_step n =
      Core.Staged.stage @@
        fun () -> start_server (n/2)
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
    let (let/) = Linocaml.bind
       
    let pingpong =
      (a --> b) msg @@
      (b --> a) msg @@ finish

    let chameleons =
      choice_at b (to_a left_or_right)
        (b, (b --> a) (left >: get_ty a pingpong) @@ finish)
        (b, (b --> a) (right >: get_ty b pingpong) @@ finish)

    let entrypoint =
      create_shared ~kinds:[`Local,0;`Local,0] chameleons

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
      let open Linocaml in
      let open LinocamlStyle in
      if n=0 then return () else begin
        let%lin #sb1 = accept entrypoint b in
        let%lin #sb2 = accept entrypoint b in
        let%lin #g = gen_with_kinds [Med.medium;Med.medium;] pingpong in
        let%lin #g,#ta = g <@ get_ch a in
        let%lin #g,#tb = g <@ get_ch b in
        let/ () = g <@ degen in
        let%lin #sb1 = (sb1@*ta) <@ deleg_send (fun x -> x#role_A#left) in
        let/ () = sb1 <@ close in
        let%lin #sb2 = (sb2@*tb) <@ deleg_send (fun x -> x#role_A#right) in
        let/ () = sb2 <@ close in
        start_server (n-1)
      end


    let start_client i () =
      let open Linocaml in
      let open LinocamlStyle in
      let debug str =
        (* Printf.printf "(%d): %s\n" i str;
         * flush stdout; *)
        ()
      in
      debug "connecting..";
      let/ () = lift @@ IO.yield () in
      let%lin #sa = connect entrypoint a in
      debug "connected.";
      match%lin sa <@ receive (fun x->x#role_B) with
      | `left(#sa2,#sa) ->
         debug "left.";
         let%lin #sa2 = sa2 <@ send (fun x->x#role_B#msg) () in
         let%lin `msg(_,#sa2) = sa2 <@ receive (fun x ->x#role_B) in
         let/ () = sa2 <@ close in
         sa <@ close
      | `right(#sb2,#sa) ->
         debug "right.";
         let%lin `msg(_,#sb2) = sb2 <@ receive (fun x->x#role_A) in
         let%lin #sb2 = sb2 <@ send (fun x ->x#role_A#msg) () in
         let/ () = sb2 <@ close in
         sa <@ close

    let rec loop f x = IO.bind (f x) (fun () -> loop f x)

    let setup n =
      (* if Med.medium = `IPCProcess || M.is_direct then begin *)
      let _ : unit list =
        List.init n begin fun i ->
          ignore (Thread.create
            (fun () ->
              (* Printf.printf "thread %d started\n" i; *)
              loop (Linocaml.run (start_client i)) ()) ())
          end
      in
      ()


    let server_step _ _ =
      IO.return ()

    let client_step n =
      Core.Staged.stage @@
        fun () -> Linocaml.run (fun () -> start_server (n/2)) ()
  end

  include MakeTestBase(Test)(Med)()
end[@@inline]
