open Mpst2.BasicCombinators
open Mpst2.Unicast
open Rows
open OUnit

module Util = struct
  [%%declare_roles a, b, c, d]
  [%%declare_labels msg, left, right, middle, ping, pong, fini]
end

open Util

let test_projection_success () =
  assert_equal ~msg:"simple" ()
  @@ ignore
  @@ extract
  @@ [%choice_at
       a ((a --> b) left finish, (a --> b) right finish, (a --> b) middle finish)];
  assert_equal ~msg:"simple loop" ()
  @@ ignore
  @@ extract
  @@ fix_with [ a; b ] (fun t -> (a --> b) msg t);
  assert_equal ~msg:"simple loop 2" ()
  @@ ignore
  @@ extract
  @@ fix_with [ a; b; c ] (fun t ->
         (choice_at a) ## (a, (a --> b) left @@ (b --> c) middle t)
           (a, (a --> b) right @@ (b --> c) middle t))

let suite =
  "Protcol combinators expected"
  >::: [ "test_projection_success" >:: test_projection_success ]
;;

let _results = run_test_tt_main suite in
()
