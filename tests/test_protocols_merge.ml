open Mpst2.GlobalCombinators
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

let test_projection_success () =
  assert_equal () @@ ignore @@ extract @@ (a --> b) msg finish;
  assert_equal ~msg:"simple" ()
  @@ ignore
  @@ extract
  @@ choice_at a
       (to_b [%disj left, right])
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
           (to_b [%disj left, right])
           (a, (a --> b) left @@ (b --> c) middle t)
           (a, (a --> b) right @@ (b --> c) middle t));
  assert_equal ~msg:"simple loop 3" ()
  @@ ignore
  @@ extract
  @@ fix_with [ a; b; c ] (fun t ->
         choice_at a
           (to_b [%disj left, right])
           (a, (a --> b) left @@ (b --> c) msg t)
           (a, (a --> b) right @@ (b --> c) msg @@ (b --> c) left finish));
  assert_equal ~msg:"loop with recursive merging" ()
  @@ ignore
  @@ extract
  @@ fix_with [ a; b; c ] (fun t ->
         choice_at a
           (to_b [%disj left, right])
           (a, (a --> b) left @@ (a --> c) msg t)
           (a, (a --> b) right @@ (a --> c) msg t))

let suite =
  "Protcol combinators expected"
  >::: [ "test_projection_success" >:: test_projection_success ]
;;

let _results = run_test_tt_main suite in
()
