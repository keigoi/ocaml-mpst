open Mpst

let loop0 =
  print_endline"loop0";
  let g = gen @@ fix (fun t -> (a --> b) msg @@ t) in
  print_endline"loop0 epp a";
  let _ = get_ep a g in
  print_endline"loop0 epp b";
  let _ = get_ep b g in
  print_endline"loop0 done";
  ()

let loop1 () =
  fix (fun t ->
        choice_at a (to_b left_middle_or_right)
          (a, choice_at a (to_b left_or_middle)
              (a, (a --> b) left @@ t)
              (a, (a --> b) middle @@ t))
          (a, (a --> b) right @@ (a --> c) msg @@ finish))


let tA ea finally =
  let rec loop () =
    let ea = send (ea#role_B#left) () in
    let ea = send (ea#role_B#middle) () in
    let ea = send (ea#role_B#left) () in
    let ea = send (ea#role_B#right) () in
    let ea = send (ea#role_C#msg) () in
    finally ea
  in
  loop ()

let tB eb finally =
  let rec loop eb =
    match receive eb#role_A with
    | `left(_,eb) -> (* print_endline"left";  *)loop eb
    | `middle(_,eb) -> (* print_endline"middle";  *)loop eb
    | `right(_,eb) -> (* print_endline"right";  *)finally eb
  in
  loop eb
  
let () =
  let () = print_endline "loop1" in
  let g = gen @@ loop1 () in
  let () = print_endline "global combinator generated" in
  let ea = get_ep a g in
  print_endline "epp a done";
  let eb = get_ep b g in
  print_endline "epp b done";
  let ec = get_ep c g in
  print_endline "epp c done";
  ignore (Thread.create (fun () ->
              tA ea (fun ea -> close ea)
            ) ());
  ignore (Thread.create (fun () ->
              tB eb (fun eb -> close eb)
            ) ());
  begin
    match receive (ec#role_A) with
    | `msg(_,ec) -> close ec
  end;
  print_endline "loop1 done"

let loop2 () =
  fix (fun t ->
        choice_at a (to_b left_or_middle_right)
          (a, (a --> b) left @@ t)
          (a, choice_at a (to_b middle_or_right)
              (a, (a --> b) middle @@ t)
              (a, (a --> b) right @@ (a --> c) msg @@ t)))

let () =
  let () = print_endline "loop2" in
  let g = gen @@ loop2 () in
  print_endline "loop2 global done";
  let ea = get_ep a g in
  print_endline "epp a done";
  let eb = get_ep b g in
  print_endline "epp b done";
  let ec = get_ep c g in
  print_endline "epp c done";
  ignore (Thread.create (fun () ->
              let rec loop ea =
                tA ea (fun ea -> loop ea)
              in
              loop ea
            ) ());
  ignore (Thread.create (fun () ->
              let rec loop eb =
                tB eb (fun eb -> loop eb)
              in
              loop eb
            ) ());
  begin
    let rec loop cnt ec =
      if cnt > 0 then begin
          match receive (ec#role_A) with
          | `msg(_,ec) -> loop (cnt-1) ec
        end
      else
        print_endline "interrupt"
    in
    ignore (loop 5 ec)
  end;
  print_endline "loop2 done"

let test1 =
    let g =
      gen @@
        fix (fun t ->
        (a --> c) msg @@
          (a --> b) msg @@
            (a --> b) msg @@ t)
    in
    print_endline"test1";
    ignore (get_ep c g);
    print_endline"test1 done"
  
let test2 =
  print_endline "test2";
  let g = 
    gen @@ fix (fun t ->
      (a --> b) left @@
      choice_at a (to_b left_or_right)
        (a, t)
        (a, (a --> b) right @@ finish))
  in
  print_endline "test2 epp-a";
  ignore (get_ep a g);
  print_endline "test2 epp-a done";
  ignore (get_ep b g);
  print_endline "test2 done";
  ()
  
let test3 =
  print_endline "test3";
  let g = 
    gen @@ fix (fun t ->
      (a --> b) right @@
      fix (fun u ->    
          choice_at a (to_b left_or_right)
          (a, (a --> b) left @@ u)
          (a, t)))
  in
  ignore (get_ep a g);
  print_endline "test3 epp-a done";
  ignore (get_ep b g);
  print_endline "test3 done";
  ()
  
let test4 =
  print_endline "test4";
  try
    let bogus = fix (fun t -> fix (fun u -> t)) in
    let _g =
      (a --> b) msg @@
        bogus
    in
    ()
  with
    CamlinternalLazy.Undefined ->
    print_endline "exception correctly occurred"

let test5 =
  print_endline "test5";
  try
    let _g =
      gen @@
      choice_at a (to_b left_or_right)
        (a, (a --> b) left @@
            fix (fun t -> (a --> b) msg @@ t))
        (a, (a --> b) right @@
            (a --> c) msg @@ finish)
    in
    failwith "unexpected (test5)"
  with
    Mpst.Seq.UnguardedLoopSeq ->
    print_endline "exception correctly occured"
