open Mpst_basic
open Rows
open OUnit

module Util = struct
  [%%declare_roles_prefixed a, b, c, d]
  [%%declare_labels msg, left, right, middle, ping, pong, fini]
end

open Util

let test_failfast_bottom () =
  let bottom () = loop_with [ a; b; c ] (fun t -> t) in
  assert_raises UnguardedLoop ~msg:"bottom" (fun _ ->
      ignore @@ extract @@ bottom ());
  assert_raises UnguardedLoop ~msg:"bottom after comm" (fun _ ->
      ignore @@ extract @@ (a --> b) msg @@ bottom ());
  assert_raises UnguardedLoop ~msg:"bottom after choice" (fun _ ->
      ignore
      @@ extract
      @@ choice_at a
           [%disj role_B (left, right)]
           (a, (a --> b) left @@ (b --> c) middle @@ bottom ())
           (a, (a --> b) right @@ (b --> c) middle finish));
  assert_raises UnguardedLoop ~msg:"bottom after choice 2" (fun _ ->
      ignore
      @@ extract
      @@ choice_at a
           [%disj role_B (left, right)]
           (a, bottom ())
           (a, (a --> b) right @@ (b --> c) middle finish));
  assert_raises UnguardedLoop ~msg:"bottom after choice 3" (fun _ ->
      ignore
      @@ extract
      @@ choice_at b
           [%disj role_B (left, right)]
           (c, (c --> b) left @@ (b --> a) middle @@ bottom ())
           (c, (c --> b) right @@ (b --> a) middle finish));
  assert_raises UnguardedLoop ~msg:"bottom after choice loop" (fun _ ->
      ignore
      @@ extract
      @@ loop_with [ a; b; c ]
      @@ fun t ->
      choice_at a
        [%disj role_B (left, right)]
        (a, (a --> b) left @@ (b --> c) middle t)
        (a, (a --> b) right @@ (b --> c) middle @@ bottom ()))

let test_failfast_loop_roles () =
  assert_raises UnguardedLoop ~msg:"declared roles absent in the loop" (fun _ ->
      ignore
      @@ extract
      @@ loop_with [ a; b; c ]
      @@ fun t ->
      (* role A does not participate in the loop *)
      choice_at c
        [%disj role_B (left, right)]
        (c, (c --> b) left t)
        (c, (c --> b) right t));
  assert_raises UnguardedLoop ~msg:"declared roles absent in the loop 2"
    (fun _ ->
      ignore
      @@ extract
      @@ choice_at b
           [%disj role_A (left, right)]
           ( b,
             (b --> a) left
             @@ loop_with [ a; b; c ]
             @@ fun t ->
             (* role A does not participate in the loop *)
             choice_at c
               [%disj role_B (left, right)]
               (c, (c --> b) left t)
               (c, (c --> b) right t) )
           ( b,
             (b --> a) right
             @@ loop_with [ a; b; c ]
             @@ fun t ->
             choice_at c
               [%disj role_B (left, right)]
               (c, (c --> b) left t)
               (c, (c --> b) right t) ));
  assert_raises UnguardedLoop ~msg:"declared roles absent in the loop 3"
    (fun _ ->
      let cd =
        loop_with [ a; b; c; d ] @@ fun t ->
        (* role A and B do not participate in the loop *)
        choice_at c
          [%disj role_D (left, right)]
          (c, (c --> d) left t)
          (c, (c --> d) right t)
      in
      ignore
      @@ extract
      @@ choice_at b [%disj role_A (left, right)] (b, cd) (b, (b --> a) right cd))

let test_loop_merge () =
  assert_raises UnguardedLoop ~msg:"loop merge" (fun _ ->
      ignore
      @@ extract
      @@ choice_at a
           [%disj role_B (left, right)]
           ( a,
             (a --> b) left
             @@ loop_with [ a; b; c ] (fun t ->
                    choice_at a
                      [%disj role_B (left, right)]
                      (a, (a --> b) left t)
                      (a, (a --> b) right t)) )
           (a, (a --> b) right @@ (a --> b) left @@ (b --> c) left finish))

let suite =
  "Protcol combinators fail-fast test"
  >::: [
         "test_failfast_bottom" >:: test_failfast_bottom;
         "test_failfast_loop_roles" >:: test_failfast_loop_roles;
         "test_loop_merge" >:: test_loop_merge;
       ]
;;

let _results = run_test_tt_main suite in
()
