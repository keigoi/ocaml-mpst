open Mpst.BasicCombinators
open Mpst.Unicast
open Mpst.Multicast
open Rows
open OUnit

module Util = struct
  [%%declare_roles_prefixed a, b, c, d]
  [%%declare_labels msg, left, right, middle, ping, pong, fini]
end

open Util

let test_run_infinite_loop () =
  assert_equal ()
  @@
  let _g0 =
    extract_with [ P (b, 5) ]
    @@ choice_at a
         [%disj role_B (left, right)]
         (a, loop_with [ a; b ] (fun t -> (a -->@@ b) left t))
         (a, loop_with [ a; b ] (fun t -> (a -->@@ b) right t))
  in
  let (`cons (sa, `cons (sb, #nil))) = _g0 in
  let ta =
    Thread.create
      (fun () ->
        let rec f sa i =
          if i > 10000 then ()
          else
            let sa = scatter sa#role_B#left in
            f sa (i + 1)
        in
        f (sa :> < role_B : < left : 'b scatter > > as 'b) 0
        (* exit *))
      ()
  in
  let _tb =
    let run i sb =
      Thread.create
        (fun () ->
          Printf.printf "Thread %d started\n" i;
          let rec f sb i =
            match branch sb#role_A with
            | `left sb -> f sb (i + 1)
            | `right sb -> f sb (i + 1)
          in
          f sb 0)
        ()
    in
    List.mapi run (get_many sb)
  in
  Thread.join ta

let suite =
  "Running multicast communication"
  >::: [ "test_run_infinite_loop" >:: test_run_infinite_loop ]
;;

let _results = run_test_tt_main suite in
()
