open Mpst2.BasicCombinators
open Mpst2.Comm
open Rows
open OUnit

module Util = struct
  [%%declare_roles_prefixed a, b, c, d]
  [%%declare_labels msg, left, right, middle, ping, pong, fini]

  let to_ m r1 r2 r3 =
    let ( ! ) x = x.role_label in
    {
      disj_concat =
        (fun l r ->
          !r1.make_obj (m.disj_concat (!r2.call_obj l) (!r3.call_obj r)));
      disj_splitL = (fun lr -> !r2.make_obj (m.disj_splitL @@ !r1.call_obj lr));
      disj_splitR = (fun lr -> !r3.make_obj (m.disj_splitR @@ !r1.call_obj lr));
    }

  let to_a m = to_ m a a a
  let to_b m = to_ m b b b
  let to_c m = to_ m c c c
  let to_d m = to_ m d d d
end

open Util

let test_failfast_bottom () =
  let bottom () = fix_with [ a; b; c ] (fun t -> t) in
  assert_raises UnguardedLoop ~msg:"bottom" (fun _ ->
      ignore @@ extract @@ bottom ());
  assert_raises UnguardedLoop ~msg:"bottom after comm" (fun _ ->
      ignore @@ extract @@ (a --> b) msg @@ bottom ());
  assert_raises UnguardedLoop ~msg:"bottom after choice" (fun _ ->
      ignore
      @@ extract
      @@ choice_at a
           (to_b [%disj left, right])
           (a, (a --> b) left @@ (b --> c) middle @@ bottom ())
           (a, (a --> b) right @@ (b --> c) middle finish));
  assert_raises UnguardedLoop ~msg:"bottom after choice 2" (fun _ ->
      ignore
      @@ extract
      @@ choice_at a
           (to_b [%disj left, right])
           (a, bottom ())
           (a, (a --> b) right @@ (b --> c) middle finish));
  assert_raises UnguardedLoop ~msg:"bottom after choice 3" (fun _ ->
      ignore
      @@ extract
      @@ choice_at b
           (to_b [%disj left, right])
           (c, (c --> b) left @@ (b --> a) middle @@ bottom ())
           (c, (c --> b) right @@ (b --> a) middle finish));
  assert_raises UnguardedLoop ~msg:"bottom after choice loop" (fun _ ->
      ignore
      @@ extract
      @@ fix_with [ a; b; c ]
      @@ fun t ->
      choice_at a
        (to_b [%disj left, right])
        (a, (a --> b) left @@ (b --> c) middle t)
        (a, (a --> b) right @@ (b --> c) middle @@ bottom ()))

let test_failfast_loop_roles () =
  assert_raises UnguardedLoop ~msg:"declared roles absent in the loop" (fun _ ->
      ignore
      @@ extract
      @@ fix_with [ a; b; c ]
      @@ fun t ->
      (* role A does not participate in the loop *)
      choice_at c
        (to_b [%disj left, right])
        (c, (c --> b) left t)
        (c, (c --> b) right t));
  assert_raises UnguardedLoop ~msg:"declared roles absent in the loop 2"
    (fun _ ->
      ignore
      @@ extract
      @@ choice_at b
           (to_a [%disj left, right])
           ( b,
             (b --> a) left
             @@ fix_with [ a; b; c ]
             @@ fun t ->
             (* role A does not participate in the loop *)
             choice_at c
               (to_b [%disj left, right])
               (c, (c --> b) left t)
               (c, (c --> b) right t) )
           ( b,
             (b --> a) right
             @@ fix_with [ a; b; c ]
             @@ fun t ->
             choice_at c
               (to_b [%disj left, right])
               (c, (c --> b) left t)
               (c, (c --> b) right t) ));
  assert_raises UnguardedLoop ~msg:"declared roles absent in the loop 3"
    (fun _ ->
      let cd =
        fix_with [ a; b; c; d ] @@ fun t ->
        (* role A and B do not participate in the loop *)
        choice_at c
          (to_d [%disj left, right])
          (c, (c --> d) left t)
          (c, (c --> d) right t)
      in
      ignore
      @@ extract
      @@ choice_at b (to_a [%disj left, right]) (b, cd) (b, (b --> a) right cd))

let test_loop_merge () =
  assert_raises UnguardedLoop ~msg:"loop merge" (fun _ ->
      ignore
      @@ extract
      @@ choice_at a
           (to_b [%disj left, right])
           ( a,
             (a --> b) left
             @@ fix_with [ a; b; c ] (fun t ->
                    choice_at a
                      (to_b [%disj left, right])
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
