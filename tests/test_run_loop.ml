open Mpst2.BasicCombinators
open Mpst2.Unicast
open Rows
open OUnit

module Util = struct
  [%%declare_roles_prefixed a, b, c, d]
  [%%declare_labels msg, left, right, middle, ping, pong, fini]
end

open Util

let test_run_infinite_loop () =
  assert_equal ()
  @@
  let _g0 =
    extract
    @@ choice_at a
         [%disj role_B (left, right)]
         (a, fix_with [ a; b ] (fun t -> (a --> b) left t))
         (a, fix_with [ a; b ] (fun t -> (a --> b) right t))
  in
  let (`cons (sa, `cons (sb, _))) = _g0 in
  let ta =
    Thread.create
      (fun () ->
        let rec f sa i =
          if i > 10000 then ()
          else
            let sa = select sa#role_B#left in
            f sa (i + 1)
        in
        f (sa :> < role_B : < left : 'b select > > as 'b) 0
        (* exit *))
      ()
  in
  let _tb =
    Thread.create
      (fun () ->
        let rec f sb i =
          match branch sb#role_A with
          | `left sb -> f sb (i + 1)
          | `right sb -> f sb (i + 1)
        in
        f sb 0)
      ()
  in
  Thread.join ta

let test_run_infinite_loop2 () =
  assert_equal ()
  @@
  let _g0 = extract @@ fix_with [ a; b ] (fun t -> (a ==> b) @@ (a ==> b) t) in
  let (`cons (sa, `cons (sb, _))) = _g0 in
  let ta =
    Thread.create
      (fun () ->
        let rec f sa i =
          if i > 10000 then ()
          else
            let sa = send sa#role_B 100 in
            f sa (i + 1)
        in
        f sa 0
        (* exit *))
      ()
  in
  let _tb =
    Thread.create
      (fun () ->
        let rec f sb i =
          let _, sb = receive sb#role_A in
          f sb (i + 1)
        in
        f sb 0)
      ()
  in
  Thread.join ta

let test_run_infinite_input_merge () =
  assert_equal ()
  @@
  let g =
    extract
    @@ choice_at a
         [%disj role_B (left, right)]
         (* c receives the same label -- we must ensure the recursive merging for C is terminating *)
         (a, fix_with [ a; b; c ] (fun t -> (a --> b) left @@ (a --> c) msg t))
         (a, fix_with [ a; b; c ] (fun t -> (a --> b) right @@ (a --> c) msg t))
  in
  let (`cons (_sa, `cons (_sb, `cons (_sc, _)))) = g in
  let ta =
    Thread.create
      (fun () ->
        let rec loop sa i =
          if i > 100 then () (* exit *)
          else
            let sa = select sa#role_B#left in
            let sa = select sa#role_C#msg in
            loop sa (i + 1)
        in
        loop
          (_sa
            :> < role_B : < left : < role_C : < msg : 'a select > > select > >
               as
               'a)
          0)
      ()
  in
  let _tb =
    Thread.create
      (fun () ->
        let rec loop sb i =
          match branch sb#role_A with
          | `left sb -> loop sb (i + 1)
          | `right sb -> loop sb (i + 1)
        in
        loop _sb 0)
      ()
  in
  let _tc =
    Thread.create
      (fun () ->
        let rec loop sc =
          let (`msg sc) = branch sc#role_A in
          loop sc
        in
        loop _sc)
      ()
  in
  Thread.join ta

let test_run_unbalanced_choice () =
  assert_equal ()
  @@
  let g =
    extract
    @@ fix_with [ a; b; c ] (fun t ->
           choice_at a
             [%disj role_B (left, right)]
             (a, (a --> b) left t)
             (a, (a --> b) right @@ (b --> c) right finish))
  in
  let (`cons (sa, `cons (sb, `cons (sc, _)))) = g in
  let _ta =
    Thread.create
      (fun () ->
        let rec loop sa i =
          if i < 1000 then
            let sa = select sa#role_B#left in
            loop sa (i + 1)
          else close (select sa#role_B#right)
        in
        loop sa 0)
      ()
  in
  let _tb =
    Thread.create
      (fun () ->
        let rec loop sb acc =
          match branch sb#role_A with
          | `left sb -> loop sb (acc + 1)
          | `right sb ->
              close (select sb#role_C#right);
              acc
        in
        ignore @@ loop sb 0;
        flush stdout)
      ()
  in
  let (`right sc) = branch sc#role_B in
  close sc

let test_run_unbalanced_choice_nested () =
  assert_equal ()
  @@
  let g =
    extract
    @@ fix_with [ a; b; c ] (fun t ->
           choice_at a
             [%disj role_B ([ left; middle ], right)]
             ( a,
               choice_at a
                 [%disj role_B (left, middle)]
                 (a, (a --> b) left t)
                 (a, (a --> b) middle t) )
             (a, (a --> b) right @@ (a --> c) msg finish))
  in
  let (`cons ((sa : 'ta), `cons ((sb : 'tb), `cons ((sc : 'tc), _)))) = g in
  let t1 =
    Thread.create
      (fun () ->
        let rec loop (sa : 'ta) i =
          if i < 5 then loop (select sa#role_B#left) (i + 1)
          else if i < 10 then loop (select sa#role_B#middle) (i + 1)
          else select (select sa#role_B#right)#role_C#msg
        in
        loop sa 0)
      ()
  in
  let t2 =
    Thread.create
      (fun () ->
        let rec loop (sb : 'tb) =
          match branch sb#role_A with
          | `left sb -> loop sb
          | `middle sb -> loop sb
          | `right sb -> close sb
        in
        loop sb)
      ()
  in
  let t3 =
    Thread.create
      (fun () ->
        let loop (sc : 'tc) =
          let (`msg sc) = branch sc#role_A in
          close sc
        in
        loop sc)
      ()
  in
  List.iter Thread.join [ t1; t2; t3 ]

let test_run_unbalanced_choice_nested2 () =
  assert_equal ()
  @@
  let g =
    extract
    @@ fix_with [ a; b; c ] (fun t1 ->
           choice_at a
             [%disj role_B (left, [ middle; right ])]
             (a, (a --> b) left @@ (a --> c) left finish)
             ( a,
               fix_with [ a; b; c ] (fun t2 ->
                   choice_at a
                     [%disj role_B (middle, right)]
                     (a, (a --> b) middle @@ (a --> c) middle t2)
                     (a, (a --> b) right @@ t1)) ))
  in
  let (`cons ((sa : 'sa9), `cons ((sb : 'sb9), `cons ((sc : 'sc9), _)))) = g in
  let ta =
    Thread.create
      (fun () ->
        let rec loop1 (sa : 'sa9) x =
          match x with
          | _ when x < 0 -> select (select sa#role_B#left)#role_C#left
          | 1 -> loop2 (select (select sa#role_B#middle)#role_C#middle) (x - 1)
          | _ -> loop1 (select sa#role_B#right) (x - 1)
        and loop2 sa x =
          match x with
          | _ when x > 0 ->
              loop2 (select (select sa#role_B#middle)#role_C#middle) (x - 1)
          | _ -> loop1 (select sa#role_B#right) (x - 1)
        in
        loop1 sa 1)
      ()
  and tb =
    Thread.create
      (fun () ->
        let rec loop (sb : 'sb9) =
          match branch sb#role_A with
          | `left sb -> close sb
          | `middle sb -> loop sb
          | `right sb -> loop sb
        in
        loop sb)
      ()
  and tc =
    Thread.create
      (fun () ->
        let rec loop (sc : 'sc9) =
          match branch sc#role_A with
          | `left sc -> close sc
          | `middle sc -> loop sc
        in
        loop sc)
      ()
  in
  List.iter Thread.join [ ta; tb; tc ]

let test_run_unguarded_choice_alternative () =
  assert_equal ()
  @@
  let g =
    extract
    @@ fix_with [ a; b ] (fun t ->
           (a --> b) left
           @@ fix_with [ a; b ] (fun u ->
                  choice_at a
                    [%disj role_B (left, right)]
                    (a, t)
                    (a, (a --> b) right @@ u)))
  in
  let (`cons ((sa : 'sa7), `cons ((sb : 'sb7), _))) = g in
  let ta =
    Thread.create
      (fun () ->
        let rec loop sa i =
          if i < 5 then loop (select sa#role_B#right) (i + 1)
          else if i < 10 then loop (select sa#role_B#left) (i + 1)
          else ()
        in
        loop (select sa#role_B#left) 0)
      ()
  in
  let _tb =
    Thread.create
      (fun () ->
        let rec loop (sb : 'sb7) =
          match branch sb#role_A with
          | `left sb -> loop sb
          | `right sb -> loop sb
        in
        loop sb)
      ()
  in
  Thread.join ta

let test_run_unguarded_choice_alternative_unbalanced () =
  let g =
    extract
    @@ fix_with [ a; b; c ] (fun t ->
           (a --> b) left
           @@ choice_at a
                [%disj role_B (left, right)]
                (a, t)
                (a, (a --> b) right @@ (b --> c) right @@ finish))
  in
  let (`cons (sa, `cons (sb, `cons (sc, _)))) = g in
  let _ta =
    Thread.create
      (fun () ->
        let rec loop sa i =
          if i < 10 then loop (select sa#role_B#left) (i + 1)
          else close (select sa#role_B#right)
        in
        loop (select sa#role_B#left) 0)
      ()
  in
  let _tb =
    Thread.create
      (fun () ->
        let rec loop sb =
          match branch sb#role_A with
          | `left sb -> loop sb
          | `right sb -> close (select sb#role_C#right)
        in
        loop sb)
      ()
  in
  let (`right sc) = branch sc#role_B in
  close sc

let test_run_partially_unguarded_choice_alternative () =
  assert_equal ()
  @@
  let g =
    extract
    @@ fix_with [ a; b; c ] (fun t ->
           (a --> b) msg
           @@ choice_at a
                [%disj role_C (left, right)]
                (* b doesn't occur here *)
                (a, (a --> c) left @@ (c --> a) msg t)
                (a, (a --> c) right @@ (c --> a) msg t))
  in
  let (`cons (sa, `cons (sb, `cons (sc, _)))) = g in
  let _t1 =
    Thread.create
      (fun () ->
        let rec loop sa i =
          let sa = select sa#role_B#msg in
          if i < 5 then
            let (`msg sa) = branch (select sa#role_C#left)#role_C in
            loop sa (i + 1)
          else if i < 10 then
            let (`msg sa) = branch (select sa#role_C#right)#role_C in
            loop sa (i + 1)
          else print_endline "g3: t1: stop"
        in
        loop sa 0)
      ()
  in
  let _t2 =
    Thread.create
      (fun () ->
        let rec loop sb =
          let (`msg sb) = branch sb#role_A in
          loop sb
        in
        loop sb)
      ()
  in
  let _t3 =
    Thread.create
      (fun () ->
        let rec loop sc =
          match branch sc#role_A with
          | `left sc -> loop (select sc#role_A#msg)
          | `right sc -> loop (select sc#role_A#msg)
        in
        loop sc)
      ()
  in
  ()

let test_run_fourparty () =
  let run i =
    let g =
      extract
      @@ choice_at d
           [%disj role_C ([ left; middle ], right)]
           ( d,
             choice_at d
               [%disj role_C (left, middle)]
               ( d,
                 (d --> c) left
                 @@ (d --> b) left
                 @@ (c --> a) left
                 @@ (a --> b) left finish )
               ( d,
                 (d --> c) middle
                 @@ (d --> b) left
                 @@ (c --> a) right
                 @@ (a --> b) right finish ) )
           ( d,
             (d --> c) right
             @@ (d --> b) right
             @@ (c --> a) right
             @@ (a --> b) right finish )
    in
    let (`cons (sa, `cons (sb, `cons (sc, `cons (sd, _))))) = g in
    let ta =
      Thread.create
        (fun () ->
          match branch sa#role_C with
          | `left sa -> select sa#role_B#left
          | `right sa -> select sa#role_B#right)
        ()
    in
    let tb =
      Thread.create
        (fun () ->
          match branch sb#role_D with
          | `left sb -> begin
              match branch sb#role_A with
              | `left sb -> close sb
              | `right sb -> close sb
            end
          | `right sb -> begin
              match branch sb#role_A with `right sb -> close sb
            end)
        ()
    in
    let tc =
      Thread.create
        (fun () ->
          match branch sc#role_D with
          | `left sc -> select sc#role_A#left
          | `middle sc -> select sc#role_A#right
          | `right sc -> select sc#role_A#right)
        ()
    in
    let td =
      Thread.create
        (fun () ->
          if i = 0 then select (select sd#role_C#left)#role_B#left
          else if i = 1 then select (select sd#role_C#middle)#role_B#left
          else select (select sd#role_C#right)#role_B#right)
        ()
    in
    List.iter Thread.join [ ta; tb; tc; td ]
  in
  assert_equal () (ignore @@ List.init 10 (fun _ -> run 0));
  assert_equal () (ignore @@ List.init 10 (fun _ -> run 1));
  assert_equal () (ignore @@ List.init 10 (fun _ -> run 2))

let suite =
  "Running mpst communication"
  >::: [
         "test_run_infinite_loop" >:: test_run_infinite_loop;
         "test_run_infinite_loops" >:: test_run_infinite_loop2;
         "test_run_infinite_input_merge" >:: test_run_infinite_input_merge;
         "test_run_unbalanced_choice" >:: test_run_unbalanced_choice;
         "test_run_unbalanced_choice_nested"
         >:: test_run_unbalanced_choice_nested;
         "test_run_unbalanced_choice_nested2"
         >:: test_run_unbalanced_choice_nested2;
         "test_run_unguarded_choice_alternative"
         >:: test_run_unguarded_choice_alternative;
         "test_run_unguarded_choice_alternative_unbalanced"
         >:: test_run_unguarded_choice_alternative_unbalanced;
         "test_run_partially_unguarded_choice_alternative"
         >:: test_run_partially_unguarded_choice_alternative;
         "test_run_fourparty" >:: test_run_fourparty;
       ]
;;

let _results = run_test_tt_main suite in
()
