open Mpst.BasicCombinators
open Mpst.Unicast
open Rows
open OUnit

module Util = struct
  [%%declare_roles a, b, c, d]
  [%%declare_labels msg, left, right, middle, ping, pong, fini]
end

open Util

let test_projection_success () =
  assert_equal () @@ ignore @@ extract @@ (a --> b) msg finish;
  assert_equal ~msg:"simple" ()
  @@ ignore
  @@ extract
  @@ choice_at a
       [%disj b (left, right)]
       (a, (a --> b) left finish)
       (a, (a --> b) right finish);
  assert_equal ~msg:"simple loop" ()
  @@ ignore
  @@ extract
  @@ fix_with [ a; b ] (fun t -> (a --> b) msg t);
  assert_equal ~msg:"simple loop 2" ()
  @@ ignore
  @@ extract
  @@ fix_with [ a; b; c ] (fun t ->
         choice_at a
           [%disj b (left, right)]
           (a, (a --> b) left @@ (b --> c) middle t)
           (a, (a --> b) right @@ (b --> c) middle t));
  assert_equal ~msg:"simple loop 3" ()
  @@ ignore
  @@ extract
  @@ fix_with [ a; b; c ] (fun t ->
         choice_at a
           [%disj b (left, right)]
           (a, (a --> b) left @@ (b --> c) msg t)
           (a, (a --> b) right @@ (b --> c) msg @@ (b --> c) left finish));
  assert_equal ~msg:"loop with recursive merging" ()
  @@ ignore
  @@ extract
  @@ fix_with [ a; b; c ] (fun t ->
         choice_at a
           [%disj b (left, right)]
           (a, (a --> b) left @@ (a --> c) msg t)
           (a, (a --> b) right @@ (a --> c) msg t));
  assert_equal ~msg:"undirected choice" ()
  @@ ignore
  @@ extract
  @@ choice_at a [%disj b, c]
       (a, (a --> b) left @@ (a --> c) msg finish)
       (a, (a --> c) right @@ (a --> b) msg finish)

let suite =
  "Protcol combinators expected"
  >::: [ "test_projection_success" >:: test_projection_success ]
;;

let _results = run_test_tt_main suite in
()
