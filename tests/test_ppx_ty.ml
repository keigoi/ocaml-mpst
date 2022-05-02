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
  @@ [%choice_at
       a
         ( [%choice_at a ((a --> b) left finish, (a --> b) right finish)],
           [%choice_at
             a
               ( (a --> b) middle finish,
                 [%choice_at a ((a --> b) ping finish, (a --> b) pong finish)]
               )] )];
  assert_equal ~msg:"simple loop" ()
  @@ ignore
  @@ extract
  @@ loop_with [ a; b ] (fun t -> (a --> b) msg t);
  assert_equal ~msg:"simple loop 2" ()
  @@ ignore
  @@ extract
  @@ loop_with [ a; b; c ] (fun t ->
         [%choice_at
           a
             ((a --> b) left @@ (b --> c) middle t)
             ((a --> b) right @@ (b --> c) middle t)]);
  assert_equal ~msg:"simple loop 3" ()
  @@ ignore
  @@ extract
  @@ loop_with [ a; b; c ] (fun t ->
         [%choice_at
           a
             ((a --> b) left @@ (b --> c) msg t)
             ((a --> b) right @@ (b --> c) msg @@ (b --> c) left finish)]);
  assert_equal ~msg:"loop with recursive merging" ()
  @@ ignore
  @@ extract
  @@ loop_with [ a; b; c ] (fun t ->
         [%choice_at
           a
             ((a --> b) left @@ (a --> c) msg t)
             ((a --> b) right @@ (a --> c) msg t)]);
  assert_equal ~msg:"undirected choice" ()
  @@ ignore
  @@ extract
  @@ [%choice_at
       a
         ((a --> b) left @@ (a --> c) msg finish)
         ((a --> c) right @@ (a --> b) msg finish)]

let test_failfast_bottom () =
  let bottom () = loop_with [ a; b; c ] (fun t -> t) in
  assert_raises UnguardedLoop ~msg:"bottom" (fun _ ->
      ignore @@ extract @@ bottom ());
  assert_raises UnguardedLoop ~msg:"bottom after comm" (fun _ ->
      ignore @@ extract @@ (a --> b) msg @@ bottom ());
  assert_raises UnguardedLoop ~msg:"bottom after choice" (fun _ ->
      ignore
      @@ extract
      @@ [%choice_at
           a
             ((a --> b) left @@ (b --> c) middle @@ bottom ())
             ((a --> b) right @@ (b --> c) middle finish)]);
  (* assert_raises UnguardedLoop ~msg:"bottom after choice 2" (fun _ ->
      ignore
      @@ extract
      @@ [%choice_at a (bottom ()) ((a --> b) right @@ (b --> c) middle finish)]); *)
  assert_raises UnguardedLoop ~msg:"bottom after choice 3" (fun _ ->
      ignore
      @@ extract
      @@ [%choice_at
           c
             ((c --> b) left @@ (b --> a) middle @@ bottom ())
             ((c --> b) right @@ (b --> a) middle finish)]);
  assert_raises UnguardedLoop ~msg:"bottom after choice loop" (fun _ ->
      ignore
      @@ extract
      @@ loop_with [ a; b; c ]
      @@ fun t ->
      [%choice_at
        a
          ((a --> b) left @@ (b --> c) middle t)
          ((a --> b) right @@ (b --> c) middle @@ bottom ())])

let test_failfast_loop_roles () =
  assert_raises UnguardedLoop ~msg:"declared roles absent in the loop" (fun _ ->
      ignore
      @@ extract
      @@ loop_with [ a; b; c ]
      @@ fun t ->
      (* role A does not participate in the loop *)
      [%choice_at c ((c --> b) left t) ((c --> b) right t)]);
  assert_raises UnguardedLoop ~msg:"declared roles absent in the loop 2"
    (fun _ ->
      let g =
        [%choice_at b
          ( 
            (b --> a) left
            @@ loop_with [ a; b; c ] (fun t ->
                   (* role A does not participate in the loop *)
                   [%choice_at c ((c --> b) left t) ((c --> b) right t)]) )
          ( 
            (b --> a) right
            @@ loop_with [ a; b; c ] (fun t ->
                   [%choice_at c ((c --> b) left t) ((c --> b) right t)]) )]
      in
      ignore @@ extract g)

let test_loop_merge () =
  assert_raises UnguardedLoop ~msg:"loop merge" (fun _ ->
      ignore
      @@ extract
      @@
      let g =
        [%choice_at
          a
            ( (a --> b) left
              @@ loop_with [ a; b; c ] (fun t ->
                     [%choice_at a ((a --> b) left t, (a --> b) right t)]),
              (a --> b) right @@ (a --> b) left @@ (b --> c) left finish )]
      in
      g)

let suite =
  "Protcol combinators expected"
  >::: [
         (* "test_projection_success" >:: test_projection_success; *)
         (* "test_failfast_bottom" >:: test_failfast_bottom; *)
         "test_failfast_loop_roles" >:: test_failfast_loop_roles;
         "test_loop_merge" >:: test_loop_merge;
       ]
;;

let _results = run_test_tt_main suite in
()
