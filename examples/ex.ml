open Must.Types
open Must.Channel_vectors
open Must.Global_combinators

module Util = struct
  let a = {role_label={make_obj=(fun v->object method role_A=v end);
                     call_obj=(fun o->o#role_A)};
         role_index=Zero}
  let b = {role_label={make_obj=(fun v->object method role_B=v end);
                      call_obj=(fun o->o#role_B)};
          role_index=Succ Zero}
  let c = {role_label={make_obj=(fun v->object method role_C=v end);
                      call_obj=(fun o->o#role_C)};
          role_index=Succ (Succ Zero)}
  let d = {role_label={make_obj=(fun v->object method role_D=v end);
                      call_obj=(fun o->o#role_D)};
          role_index=Succ (Succ (Succ Zero))}

  let msg =
    {obj={make_obj=(fun f -> object method msg=f end);
          call_obj=(fun o -> o#msg)};
    var={make_var=(fun v -> `msg(v));
          match_var=(function `msg(v) -> Some v | _ -> None)}}
  let left =
    {obj={make_obj=(fun f -> object method left=f end);
          call_obj=(fun o -> o#left)};
    var={make_var=(fun v -> `left(v));
          match_var=(function `left(v) -> Some v | _ -> None)}}
  let right =
    {obj={make_obj=(fun f -> object method right=f end);
          call_obj=(fun o -> o#right)};
    var={make_var=(fun v -> `right(v));
          match_var=(function `right(v) -> Some v | _ -> None)}}
  let middle =
    {obj={make_obj=(fun f -> object method middle=f end);
          call_obj=(fun o -> o#middle)};
    var={make_var=(fun v -> `middle(v));
          match_var=(function `middle(v) -> Some v | _ -> None)}}
  let ping =
    {obj={make_obj=(fun f -> object method ping=f end);
          call_obj=(fun o -> o#ping)};
    var={make_var=(fun v -> `ping(v));
          match_var=(function `ping(v) -> Some v | _ -> None)}}
  let pong =
    {obj={make_obj=(fun f -> object method pong=f end);
          call_obj=(fun o -> o#pong)};
    var={make_var=(fun v -> `pong(v));
          match_var=(function `pong(v) -> Some v | _ -> None)}}
  let fini =
    {obj={make_obj=(fun f -> object method fini=f end);
          call_obj=(fun o -> o#fini)};
    var={make_var=(fun v -> `fini(v));
          match_var=(function `fini(v) -> Some v | _ -> None)}}

  let left_or_right =
    {disj_concat=(fun l r -> object method left=l#left method right=r#right end);
    disj_splitL=(fun lr -> (lr :> <left : _>));
    disj_splitR=(fun lr -> (lr :> <right : _>));
    }
  let right_or_left =
    {disj_concat=(fun l r -> object method right=l#right method left=r#left end);
    disj_splitL=(fun lr -> (lr :> <right : _>));
    disj_splitR=(fun lr -> (lr :> <left : _>));
    }


  let to_ m r1 r2 r3 =
    let (!) x = x.role_label in
    {disj_concat=(fun l r -> !r1.make_obj (m.disj_concat (!r2.call_obj l) (!r3.call_obj r)));
    disj_splitL=(fun lr -> !r2.make_obj (m.disj_splitL @@ !r1.call_obj lr));
    disj_splitR=(fun lr -> !r3.make_obj (m.disj_splitR @@ !r1.call_obj lr));
    }
  let to_a m = to_ m a a a
  let to_b m = to_ m b b b
  let to_c m = to_ m c c c
  let to_d m = to_ m d d d

  let left_middle_or_right =
    {disj_concat=(fun l r -> object method left=l#left method middle=l#middle method right=r#right end);
    disj_splitL=(fun lr -> (lr :> <left : _; middle: _>));
    disj_splitR=(fun lr -> (lr :> <right : _>));
    }

  let left_or_middle =
    {disj_concat=(fun l r -> object method left=l#left method middle=r#middle end);
    disj_splitL=(fun lr -> (lr :> <left : _>));
    disj_splitR=(fun lr -> (lr :> <middle : _>));
    }

  let left_or_middle_right =
    {disj_concat=(fun l r -> object method left=l#left method middle=r#middle method right=r#right end);
    disj_splitL=(fun lr -> (lr :> <left : _>));
    disj_splitR=(fun lr -> (lr :> <middle: _; right : _>));
    }

  let middle_or_right =
    {disj_concat=(fun l r -> object method middle=l#middle method right=r#right end);
    disj_splitL=(fun lr -> (lr :> <middle : _>));
    disj_splitR=(fun lr -> (lr :> <right : _>));
    }

end

open Util
let () =
  let expect_unguarded msg g =
    begin try
      ignore (extract g);
      failwith "unexpected: it should fail!"
    with
      (UnguardedLoop str) as _e -> 
        print_endline (str ^ " (expected: " ^ msg ^ ")");
    end
  in
  let bottom () = fix_with [a;b;c] (fun t -> t) in
  let () =
    expect_unguarded "bottom" @@ bottom ()
  in
  let () =
    expect_unguarded "bottom after comm" @@ (a -->b) msg @@ bottom ()
  in
  let () =
    expect_unguarded "bottom after choice" @@
      choice_at a (to_b left_or_right)
      (a, (a --> b) left @@ (b --> c) middle @@ bottom ())
      (a, (a --> b) right @@ (b --> c) middle finish)
  in
  let () =
    expect_unguarded "bottom after choice 2" @@
      choice_at a (to_b left_or_right)
      (a, bottom ())
      (a, (a --> b) right @@ (b --> c) middle finish)
  in
  let () =
    expect_unguarded "bottom after choice 3" @@
      choice_at b (to_b left_or_right)
      (c, (c --> b) left @@ (b --> a) middle @@ bottom ())
      (c, (c --> b) right @@ (b --> a) middle finish)
  in
  let () =
    expect_unguarded "bottom after choice loop" @@
      fix_with [a;b;c] @@ fun t -> 
        choice_at a (to_b left_or_right)
        (a, (a --> b) left @@ (b --> c) middle t)
        (a, (a --> b) right @@ (b --> c) middle @@ bottom ())
  in
  let () =
    expect_unguarded "bottom after choice loop 2" @@
      fix_with [a;b;c] @@ fun t -> 
        choice_at c (to_b left_or_right)
        (c, (c --> b) left t)
        (c, (c --> b) right t)
  in
  let () =
    expect_unguarded "bottom after choice loop 3" @@
    choice_at b (to_a left_or_right)
      (b, (b --> a) left @@
          fix_with [a;b;c] @@ fun t -> 
          choice_at c (to_b left_or_right)
          (c, (c --> b) left t)
          (c, (c --> b) right t))
      (b, (b --> a) right @@
          fix_with [a;b;c] @@ fun t -> 
            choice_at c (to_b left_or_right)
            (c, (c --> b) left t)
            (c, (c --> b) right t))
  in
  let () =
    let cd = 
      fix_with [a;b;c;d] @@ fun t -> 
        choice_at c (to_d left_or_right)
        (c, (c --> d) left t)
        (c, (c --> d) right t)
    in
    expect_unguarded "bottom after choice loop 4" @@
    choice_at b (to_a left_or_right)
      (b, cd)
      (b, (b --> a) right cd)
  in
  let () =
    expect_unguarded "loop merge" @@
      choice_at a (to_b left_or_right)
      (a, (a --> b) left @@ 
          fix_with [a;b;c] (fun t -> 
            choice_at a (to_b left_or_right) 
            (a, (a --> b) left t)
            (a, (a --> b) right t)
          ))
      (a, (a --> b) right @@ (a --> b) left @@ (b --> c) left finish)
  in
  let _g =
    extract @@
      (a --> b) msg finish
  in
  let _g =
    extract @@ 
      choice_at a (to_b left_or_right)
      (a,(a --> b) left finish)
      (a,(a --> b) right finish)
  in
  let _g =
    extract @@
      fix_with [a;b] (fun t -> (a --> b) msg t)
  in
  let _g8 =
    extract @@
    fix_with [a;b;c] (fun t -> 
        choice_at a (to_b left_or_right)
        (a, (a --> b) left @@ (b --> c) middle t)
        (a, (a --> b) right @@ (b --> c) middle t))
  in
  print_endline "g7";
  let _g7 =
    extract @@
    fix_with [a;b] (fun t -> 
      (a --> b) left @@
      fix_with [a;b] (fun u -> 
        choice_at a (to_b left_or_right)
        (a, t)
        (a, (a --> b) right @@ u)
      ))
  in
  let () =
    let `cons((sa:'sa7),`cons((sb:'sb7),_)) = _g7 in
    let _ta = Thread.create (fun () -> 
        let rec loop sa i =
          print_endline "select";
          if i < 5 then begin
            print_endline "select right";
            loop (select sa#role_B#right) (i+1)
          end else if i < 10 then begin
            print_endline "select left";
            loop (select sa#role_B#left) (i+1)
          end else
            ()
        in
        loop (select sa#role_B#left) 0
      ) ()
    in
    let _tb = Thread.create (fun () -> 
        let rec loop (sb:'sb7) =
          print_endline "branch";
          match branch sb#role_A with
          | `left(sb) ->
            print_endline "g7: tb: left";
            loop sb
          | `right(sb) ->
            print_endline "g7: tb: right";
            loop sb
        in
        loop sb
        ) ()
    in
    Thread.join _ta
  in
  let _g6 =
    print_endline "g6 determinising";
    extract @@
      choice_at a (to_b left_or_right)
      (a, fix_with [a;b;c] (fun t -> (a --> b) left @@ (a --> c) msg t))
      (a, fix_with [a;b;c] (fun t -> (a --> b) right @@ (a --> c) msg t))
  in
  print_endline "g6 determinised";
  let () = 
    let `cons(_sa,`cons(_sb,`cons(_sc,_))) = _g6 in
    let _ta = Thread.create (fun () ->
      print_endline "ta start";
      let rec loop sa i =
        if i > 100 then
          ()
        else begin 
          if i mod 10 = 0 then begin
              Printf.printf "%d\n" i;
              flush stdout
          end;
          let sa = select sa#role_B#left in
          let sa = select sa#role_C#msg in
          loop sa (i+1)
        end
      in 
      loop (_sa :> (<role_B: <left: <role_C:<msg: 'a out> > out> > as 'a)) 0;
      print_endline "g6: ta finish"
      ) ()
    in
    let _tb = Thread.create (fun () ->
      print_endline "tb start";
      let rec loop sb i =
        if i mod 10 = 0 then begin
          Printf.printf "%d\n" i;
          flush stdout
        end;
        match branch sb#role_A with
        | `left sb -> 
          loop sb (i+1)
        | `right sb -> 
          loop sb (i+1)
      in
      loop _sb 0
      ) ()
    in
    let _tc = Thread.create (fun () -> 
        let rec loop sc =
          let `msg sc = branch sc#role_A in
          loop sc
        in 
        loop _sc
      ) () 
    in
    Thread.join _ta
  in
  let _g5 =
    extract @@
      fix_with [a;b;c] (fun t -> 
          choice_at a (to_b left_or_right)
          (a, (a-->b) left @@ (b-->c) msg t)
          (a, (a-->b) right @@ (b-->c) msg @@ (b-->c) left finish)
        )
  in
  let _g4 =
    extract @@
    fix_with [a;b;c] (fun t -> 
      choice_at a (to_b left_middle_or_right)
      (a, choice_at a (to_b left_or_middle)
          (a, (a --> b) left t)
          (a, (a --> b) middle t))
      (a, (a --> b) right @@ (a --> c) msg finish)
    )
  in
  let () =
    let `cons((sa:'ta),`cons((sb:'tb),`cons((sc:'tc),_))) = _g4 in
    let _t1 =
      Thread.create (fun () -> 
        let rec loop (sa:'ta) i =
          if i<5 then 
            loop (select sa#role_B#left) (i+1)
          else if i<10 then
            loop (select sa#role_B#middle) (i+1)
          else
            select (select sa#role_B#right)#role_C#msg
        in
        loop sa 0;
        print_endline "g4: ta finished"
      ) ()
    in
    let _t2 =
      Thread.create (fun () -> 
        let rec loop (sb:'tb) =
          match branch sb#role_A with
          | `left sb -> loop sb
          | `middle sb -> loop sb
          | `right sb -> close sb
        in 
        loop sb;
        print_endline "g4: tb finished"
      ) ()
    in
    let _t3 =
      Thread.create (fun () -> 
        let loop (sc:'tc) =
          let `msg sc = branch sc#role_A in
          close sc
        in 
        loop sc;
        print_endline "g4: tc finished"
      ) ()
    in
    List.iter Thread.join [_t1; _t2; _t3];
  in
  let _g3 =
    extract @@
    fix_with [a;b;c] (fun t -> 
      (a --> b) msg @@
      choice_at a (to_c left_or_right)
      (a, (a --> c) left @@ (c --> a) msg t)
      (a, (a --> c) right @@ (c --> a) msg t)
    )
  in
  let () =
    let `cons(sa,`cons(sb,`cons(sc,_))) = _g3 in
    let _t1 =
      Thread.create (fun () -> 
        let rec loop sa i =
          let sa = select sa#role_B#msg in
          if i<5 then 
            let `msg sa = branch (select sa#role_C#left)#role_C in
            loop sa (i+1)
          else if i<10 then
            let `msg sa = branch (select sa#role_C#right)#role_C in
            loop sa (i+1)
          else
            print_endline "g3: t1: stop"
        in
        loop sa 0
      ) ()
    in
    let _t2 =
      Thread.create (fun () -> 
        let rec loop sb =
          let `msg sb = branch sb#role_A in
          loop sb
        in loop sb
      ) ()
    in
    let _t3 =
      Thread.create (fun () -> 
        let rec loop sc =
          match branch sc#role_A with
          | `left sc -> loop (select sc#role_A#msg)
          | `right sc -> loop (select sc#role_A#msg)
        in loop sc
      ) ()
    in
    ()
  in
  let _g2 = 
    extract @@
    fix_with [a;b;c] (fun t ->
      (a --> b) left @@
      choice_at a (to_b left_or_right)
      (a, t)
      (a, (a --> b) right @@ (b --> c) right @@ finish)
    )
  in
  let () =
    let `cons(sa,`cons(sb, `cons(sc, _))) = _g2 in
    let _ta = Thread.create (fun () -> 
      let rec loop sa i =
        if i < 10 then
          loop (select sa#role_B#left) (i+1)
        else
          close (select sa#role_B#right)
      in
      loop (select sa#role_B#left) 0;
      print_endline "g2: ta finished"

      ) ()
    in
    let _tb = Thread.create (fun () -> 
        let rec loop sb =
          match branch sb#role_A with
          | `left sb -> loop sb
          | `right sb -> close (select sb#role_C#right)
        in
        loop sb;
        print_endline "g2: tb finished"
      ) ()
    in
    let `right sc = branch sc#role_B in
    close sc;
    print_endline "g2: tc finished"
  in  
  let _g1 =
    extract @@
    fix_with [a;b;c] (fun t ->
        choice_at a (to_b left_or_right)
        (a, (a --> b) left t)
        (a, (a --> b) right @@ (b --> c) right finish)
      )
  in
  let () =
    let `cons(sa,`cons(sb,`cons(sc,_))) = _g1 in
    let _ta = Thread.create (fun () ->
        let rec loop sa i =
          if i < 1000 then
            let sa = select sa#role_B#left in
            loop sa (i+1)
          else
            close (select sa#role_B#right)
          in
          loop sa 0;
          print_endline "g1: ta finish"
      ) () 
    in
    let _tb = Thread.create (fun () -> 
      let rec loop sb acc =
        match branch sb#role_A with
        | `left sb -> loop sb (acc+1)
        | `right sb -> 
          close (select sb#role_C#right);
          acc
      in
      Printf.printf "%d\n" @@ loop sb 0;
      flush stdout
      ) ()
    in
    let `right(sc) = branch sc#role_B in
    close sc
  in
  let _g0 =
    extract @@
      choice_at a (to_b left_or_right)
      (a, fix_with [a;b] (fun t -> (a --> b) left t))
      (a, fix_with [a;b] (fun t -> (a --> b) right t))
  in
  let () = 
    let `cons(sa,`cons(sb,_)) = _g0 in
    let ta = Thread.create (fun () ->
      print_endline "ta start";
      let rec f sa i =
        if i > 10000 then
          ()
        else begin 
            if i mod 1000 = 0 then 
              Printf.printf "%d\n" i;
              flush stdout;
          let sa = select (sa#role_B#left) in
          f sa (i+1)
        end
      in 
      f (sa :> (<role_B: <left: 'b out> > as 'b)) 0;
      print_endline "g0: ta finish"
      ) ()
    in
    let _tb = Thread.create (fun () ->
      print_endline "tb start";
      let rec f sb i =
        (* begin 
          if i mod 10 = 0 then 
            Printf.printf "%d\n" i;
        end; *)
        match branch sb#role_A with
        | `left sb -> 
          (* print_endline "tb: left"; *)
          f sb (i+1)
        | `right sb -> 
          (* print_endline "tb: right"; *)
          f sb (i+1)
      in
      f sb 0
      ) ()
    in
    Thread.join ta
  in
  let () =
    let _g9 =
      extract @@
      fix_with [a;b;c] (fun t1 -> 
        choice_at a (to_b left_or_middle_right)
        (a, (a --> b) left @@ (a --> c) left finish)
        (a, fix_with [a;b;c] (fun t2 ->
            choice_at a (to_b middle_or_right)
            (a, (a --> b) middle @@ (a --> c) middle t2)
            (a, (a --> b) right @@ t1))))
    in
    let `cons((sa:'sa9),`cons((sb:'sb9),`cons((sc:'sc9),_))) = _g9
    in
    let _ta = Thread.create (fun () -> 
      let rec loop1 (sa:'sa9) x =
        match x with
        | _ when x < 0 -> 
          print_endline "A: select left at loop1";
          select (select sa#role_B#left)#role_C#left
        | 1 -> 
          print_endline "A: select middle at loop1";
          loop2 (select (select sa#role_B#middle)#role_C#middle) (x-1)
        | _ -> 
          print_endline "A: select right at loop1";
          loop1 (select sa#role_B#right) (x-1)
      and loop2 sa x =
        match x with
        | _ when x  > 0 -> 
          print_endline "A: select middle at loop2";
          loop2 (select (select sa#role_B#middle)#role_C#middle) (x-1)
        | _ -> 
          print_endline "A: select right at loop2";
          loop1 (select sa#role_B#right) (x-1)
      in
      loop1 sa 1
      ) ()
    and _tb = Thread.create (fun () -> 
      let rec loop (sb:'sb9) =
        match branch sb#role_A with
        | `left(sb) -> 
          print_endline "B: left";
          close sb
        | `middle(sb) -> 
          print_endline "B: middle";
          loop sb
        | `right(sb) -> 
          print_endline "B: right";
          loop sb
      in
      loop sb
      ) ()
    and _tc = Thread.create (fun () -> 
      let rec loop (sc:'sc9) =
        match branch sc#role_A with
        | `left(sc) -> print_endline "C: left"; close sc
        | `middle(sc) -> print_endline "C: middle"; loop sc
      in
      loop sc
      ) ()
    in
    List.iter Thread.join [_ta;_tb;_tc];
    ()
  in
  let () =
    let _g10 = 
      fix_with [a;b;c] (fun t -> choice_at a (to_b left_or_right) (a, (a --> b) left @@ (a --> c) msg t) (a, (a --> b) right @@ (a --> c) msg t))
    in
    let `cons(_,`cons(_,_)) = extract _g10
    in ()
  in
  print_endline "ok";
  ()
