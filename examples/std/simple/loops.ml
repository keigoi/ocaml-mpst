open Concur_shims
open Mpst
open Mpst.Util

let (let*) m f = IO.bind m (fun x -> IO.bind (IO.yield ()) (fun () -> f x))

let loop0 =
  print_endline"loop0";
  let g = gen @@ fix (fun t -> (a --> b) msg @@ t) in
  print_endline"loop0 epp a";
  let _ = get_ch a g in
  print_endline"loop0 epp b";
  let _ = get_ch b g in
  print_endline"loop0 done";
  ()

let loop2 () =
  fix (fun t ->
        choice_at a (to_b left_or_middle_right)
          (a, (a --> b) left @@ t)
          (a, choice_at a (to_b middle_or_right)
              (a, (a --> b) middle @@ t)
              (a, (a --> b) right @@ (a --> c) msg @@ t)))

let tA ea =
  let* () = IO.printl "tA start" in
  let rec loop ea =
    print_endline  "tA left1";
    let* ea = send (ea#role_B#left) () in
    print_endline  "tA middle1";
    let* ea = send (ea#role_B#middle) () in
    print_endline  "tA middle2";
    let* ea = send (ea#role_B#middle) () in
    print_endline  "tA left2";
    let* ea = send (ea#role_B#left) () in
    print_endline  "tA right1";
    let* ea = send (ea#role_B#right) () in
    print_endline  "tA msg";
    let* ea = send (ea#role_C#msg) () in
    print_endline  "tA right";
    let* ea = send (ea#role_B#right) () in
    print_endline  "tA msg";
    let* ea = send (ea#role_C#msg) () in
    loop ea
  in
  loop ea

let tB eb =
  let rec loop eb =
    let* var = receive eb#role_A in
    match var with
    | `left(_,eb) -> print_endline"left";loop eb
    | `middle(_,eb) -> print_endline"middle";loop eb
    | `right(_,eb) -> print_endline"right"; loop eb
  in
  loop eb

let () =
  let () = print_endline "loop2" in
  let g = gen @@ loop2 () in
  print_endline "loop2 global done";
  let ea = get_ch a g in
  print_endline "epp a done";
  let eb = get_ch b g in
  print_endline "epp b done";
  let ec = get_ch c g in
  print_endline "epp c done";
  let (_:Thread.t) = Thread.create tA ea in
  print_endline "thread A started";
  let (_:Thread.t) = Thread.create tB eb in
  print_endline "thread B started";
  IO.main_run @@ begin
    let* () = IO.printl "C" in
    let rec loop cnt ec =
      if cnt > 0 then begin
        let* () = IO.printl (string_of_int cnt) in
        let* var = receive (ec#role_A) in
        match var with
        | `msg(_,ec) -> loop (cnt-1) ec
      end else
        IO.printl "interrupt"
    in
    loop 5 ec
  end;
  print_endline "loop2 done"

(* let test1 =
 *     let g =
 *       gen @@
 *         fix (fun t ->
 *         (a --> c) msg @@
 *           (a --> b) msg @@
 *             (a --> b) msg @@ t)
 *     in
 *     print_endline"test1";
 *     ignore (get_ch c g);
 *     print_endline"test1 done"
 * 
 * let test2 =
 *   print_endline "test2";
 *   let g =
 *     gen @@ fix (fun t ->
 *       (a --> b) left @@
 *       choice_at a (to_b left_or_right)
 *         (a, t)
 *         (a, (a --> b) right @@ finish))
 *   in
 *   print_endline "test2 epp-a";
 *   ignore (get_ch a g);
 *   print_endline "test2 epp-a done";
 *   ignore (get_ch b g);
 *   print_endline "test2 done";
 *   ()
 * 
 * let test3 =
 *   print_endline "test3";
 *   let g =
 *     gen @@ fix (fun t ->
 *       (a --> b) right @@
 *       fix (fun u ->
 *           choice_at a (to_b left_or_right)
 *           (a, (a --> b) left @@ u)
 *           (a, t)))
 *   in
 *   ignore (get_ch a g);
 *   print_endline "test3 epp-a done";
 *   ignore (get_ch b g);
 *   print_endline "test3 done";
 *   ()
 * 
 * let test4 =
 *   print_endline "test4";
 *   try
 *     let bogus = fix (fun t -> fix (fun _ -> t)) in
 *     let _g =
 *       (a --> b) msg @@
 *         bogus
 *     in
 *     ()
 *   with
 *     CamlinternalLazy.Undefined ->
 *     print_endline "exception correctly occurred"
 * 
 * let test5 =
 *   print_endline "test5";
 *   try
 *     let _g =
 *       gen @@
 *       choice_at a (to_b left_or_right)
 *         (a, (a --> b) left @@
 *             fix (fun t -> (a --> b) msg @@ t))
 *         (a, (a --> b) right @@
 *             (a --> c) msg @@ finish)
 *     in
 *     failwith "unexpected (test5)"
 *   with
 *     UnguardedLoopSeq ->
 *     print_endline "exception correctly occured"
 * 
 * let test6 =
 *   try
 *     print_endline "test6";
 *     let _ =
 *       gen (fix (fun t ->
 *                choice_at a (to_b left_or_middle)
 *                  (a, (a --> b) left @@
 *                      choice_at a (to_b left_middle_or_right)
 *                      (a, t)
 *                      (a, (\* here C blocks indefinitely, and must be rejected *\)
 *                          fix (fun u -> (a --> b) right @@ u)))
 *                  (a, (a --> b) middle @@
 *                      (b --> c) middle @@ finish)))
 *     in
 *     failwith "unexpected (test6)"
 *   with
 *     UnguardedLoopSeq ->
 *     print_endline "exception correctly occurred"
 * 
 * let test7 =
 *   try
 *     print_endline "test7";
 *     let g =
 *       gen @@
 *       choice_at a (to_b left_or_right)
 *         (a, (a --> b) left @@ (b --> c) left finish)
 *         (a, (a --> b) right @@ fix (fun x -> (b --> c) right (closed_at a x)))
 *     in
 *     ignore (get_ch a g);
 *     ignore (get_ch b g);
 *     ignore (get_ch c g);
 *     ()
 *   with
 *     UnguardedLoopSeq ->
 *     print_endline "unexpected" *)
    

