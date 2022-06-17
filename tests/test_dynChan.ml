module DynChan = Mpst_basic.DynChan
open OUnit

let test_unify () =
  let ch = DynChan.make () in
  let n1 = DynChan.new_endpoint ch in
  let n2 = DynChan.new_endpoint ch in
  let n3 = DynChan.new_endpoint ch in
  DynChan.unify n1 n2;
  DynChan.unify n3 n2;
  let n1, _n2, n3 =
    (DynChan.finalise n1, DynChan.finalise n2, DynChan.finalise n3)
  in
  let _t = Thread.create (DynChan.send n1) () in
  assert_equal () (DynChan.receive n3)

let suite = "Running mpst communication" >::: [ "test_unify" >:: test_unify ]

let () =
  let _results = run_test_tt_main suite in
  ()
