open Mpst.BasicCombinators
open Mpst.Unicast
open Rows
open OUnit

module Util = struct
  [%%declare_roles_prefixed a, b, c, d]
  [%%declare_labels msg, left, right, middle, ping, pong, fini]
end

open Util

let test_linearity_violation () =
  assert_equal ()
  @@
  let _g0 =
    extract
    @@ choice_at a
         [%disj role_B (left, right)]
         (a, (a --> b) left finish)
         (a, (a --> b) right finish)
  in
  let (`cons (sa, `cons (sb, _))) = _g0 in
  assert_raises Mpst.Lin.InvalidEndpoint ~msg:"linearity violation" (fun _ ->
      let _ = select sa#role_B#left in
      let _ = select sa#role_B#left in
      ());
  assert_raises Mpst.Lin.InvalidEndpoint ~msg:"linearity violation" (fun _ ->
      let _ = branch sb#role_A in
      let _ = branch sb#role_A in
      ())

let suite =
  "Running mpst communication"
  >::: [ "test_linearity_violation" >:: test_linearity_violation ]
;;

let _results = run_test_tt_main suite in
()
