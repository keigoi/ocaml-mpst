open Mpst.BasicCombinators
open Mpst.Unicast
open Mpst.Multicast
open Rows
open OUnit

module Util = struct
  [%%declare_roles a, b, c, d]
  [%%declare_labels msg, left, right, middle, ping, pong, fini]
end

open Util

let extract_b_many g =
  let `cons(_,`cons(b,_)) = extract g in
  ignore (Mpst.Multicast.get_many b)

let test_multicast_projection_success () =
  let finish = many_at a Mpst.BasicCombinators.finish in
  ignore @@ extract @@ (a @@--> b) left finish;
  ignore
  @@ extract
  @@ choice_at b
       [%disj a (left, right)]
       (b, (b -->@@ a) left finish)
       (b, (b -->@@ a) right finish);
  assert_equal () @@ ignore @@ extract @@ (a @@--> b) msg finish;
  let finish = many_at b Mpst.BasicCombinators.finish in
  assert_equal ~msg:"simple" ()
  @@ ignore
  @@ extract_b_many
  @@ choice_at a
       [%disj b (left, right)]
       (a, (a -->@@ b) left finish)
       (a, (a -->@@ b) right finish);
  assert_equal ~msg:"simple loop" ()
  @@ ignore
  @@ extract_b_many
  @@ loop_with [ a; b ] (fun t -> (a -->@@ b) msg t);
  assert_equal ~msg:"simple loop 2" ()
  @@ ignore
  @@ extract_b_many
  @@ loop_with [ a; b; c ] (fun t ->
         choice_at a
           [%disj b (left, right)]
           (a, (a -->@@ b) left @@ (b @@--> c) middle t)
           (a, (a -->@@ b) right @@ (b @@--> c) middle t));
  assert_equal ~msg:"simple loop 3" ()
  @@ ignore
  @@ extract_b_many
  @@ loop_with [ a; b; c ] (fun t ->
         choice_at a
           [%disj b (left, right)]
           (a, (a -->@@ b) left @@ (b @@--> c) msg t)
           (a, (a -->@@ b) right @@ (b @@--> c) msg @@ (b @@--> c) left finish));
  assert_equal ~msg:"loop with recursive merging" ()
  @@ ignore
  @@ extract_b_many
  @@ loop_with [ a; b; c ] (fun t ->
         choice_at a
           [%disj b (left, right)]
           (a, (a -->@@ b) left @@ (a --> c) msg t)
           (a, (a -->@@ b) right @@ (a --> c) msg t))

let suite =
  "Protcol combinators expected"
  >::: [
         "test_multicast_projection_success"
         >:: test_multicast_projection_success;
       ]
;;

let _results = run_test_tt_main suite in
()
