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
  let _g0 = [%choice_at a ((a --> b) left finish, (a --> b) right finish)] in
  let (`cons (sa, `cons (sb, #nil))) = extract _g0 in
  let t =
    Thread.create
      (fun _ -> match branch sb#role_A with `left () -> () | `right () -> ())
      ()
  in
  assert_raises Mpst.Lin.InvalidEndpoint ~msg:"linearity violation" (fun _ ->
      let _ = select sa#role_B#left in
      let _ = select sa#role_B#left in
      ());
  Thread.join t

let test_linearity_violation2 () =
  assert_equal ()
  @@
  let _g0 = [%choice_at a ((a --> b) left finish, (a --> b) right finish)] in
  let (`cons (sa, `cons (sb, #nil))) = extract _g0 in
  let t =
    Thread.create
      (fun _ ->
        let () = select sa#role_B#left in
        ())
      ()
  in
  assert_raises Mpst.Lin.InvalidEndpoint ~msg:"linearity violation" (fun _ ->
      let _ = branch sb#role_A in
      let _ = branch sb#role_A in
      ());
  Thread.join t

let suite =
  "Running mpst communication"
  >::: [
         "test_linearity_violation" >:: test_linearity_violation;
         "test_linearity_violation2" >:: test_linearity_violation2;
       ]
;;

let _results = run_test_tt_main suite in
()

(* let test_linearity_violation =
   assert_equal ()
   @@
   let _g0 = [%choice_at a ((a --> b) left finish, (a --> b) right finish)] in
   let (`cons (sa, `cons (sb, #nil))) = extract _g0 in
   let t =
     Thread.create
       (fun () ->
         let _ = branch sb#role_A in
         let _ = branch sb#role_A in
         ())
       ()
   in
   let _ = select sa#role_B#left in
   let _ = select sa#role_B#left in
   Thread.join t;
   () *)
