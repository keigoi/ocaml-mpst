module Lin = Mpst.Lin
open OUnit

let test_unify () =
  let unify = Lin.merge_lin (fun () () -> ()) in
  let n = Lin.declare () in
  let n1 = Lin.fresh n in
  let n2 = Lin.fresh @@ Lin.declare () in
  let n3 = Lin.fresh @@ Lin.declare () in
  let n12 = unify n1 n2 in
  let n32 = unify n3 n2 in
  Lin.use n1;
  assert_raises Lin.InvalidEndpoint (fun () -> Lin.use n1);
  assert_raises Lin.InvalidEndpoint (fun () -> Lin.use n2);
  assert_raises Lin.InvalidEndpoint (fun () -> Lin.use n12);
  assert_raises Lin.InvalidEndpoint (fun () -> Lin.use n32);
  ignore @@ Lin.fresh n;
  Lin.use n1;
  ignore @@ Lin.fresh n;
  Lin.use n2;
  ignore @@ Lin.fresh n;
  Lin.use n3;
  ignore @@ Lin.fresh n;
  Lin.use n12;
  ignore @@ Lin.fresh n;
  Lin.use n32;
  ()

let suite = "Running mpst communication" >::: [ "test_unify" >:: test_unify ]

let () =
  let _results = run_test_tt_main suite in
  ()
