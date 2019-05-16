open Mpst_simple

let left_middle_or_right =
  {obj_merge=(fun l r -> object method left=l#left method middle=l#middle method right=r#right end);
   obj_splitL=(fun lr -> (lr :> <left : _; middle: _>));
   obj_splitR=(fun lr -> (lr :> <right : _>));
  }

let left_or_middle =
  {obj_merge=(fun l r -> object method left=l#left method middle=r#middle end);
   obj_splitL=(fun lr -> (lr :> <left : _>));
   obj_splitR=(fun lr -> (lr :> <middle : _>));
  }

let left_or_middle_right =
  {obj_merge=(fun l r -> object method left=l#left method middle=r#middle method right=r#right end);
   obj_splitL=(fun lr -> (lr :> <left : _>));
   obj_splitR=(fun lr -> (lr :> <middle: _; right : _>));
  }

let middle_or_right =
  {obj_merge=(fun l r -> object method middle=l#middle method right=r#right end);
   obj_splitL=(fun lr -> (lr :> <middle : _>));
   obj_splitR=(fun lr -> (lr :> <right : _>));
  }

let loop1 () =
  let rec g =
    lazy begin
        choice_at a (to_b left_middle_or_right)
          (a, choice_at a (to_b left_or_middle)
              (a, (a --> b) left @@ goto g)
              (a, (a --> b) middle @@ goto g))
          (a, (a --> b) right @@ (a --> c) msg @@ finish)
      end
  in
  Lazy.force g

let () =
  let () = print_endline "loop1" in
  let g = loop1 () in
  ignore @@ get_ep a g;
  print_endline "A ok";
  ignore @@ get_ep b g;
  print_endline "B ok";
  ignore @@ get_ep c g;
  print_endline "C ok";
  ()

let loop2 () =
  let rec g =
    lazy begin
        choice_at a (to_b left_or_middle_right)
          (a, (a --> b) left @@ (a --> c) msg @@ finish)
          (a, choice_at a (to_b middle_or_right)
              (a, (a --> b) middle @@ goto g)
              (a, (a --> b) right @@ goto g))
      end
  in
  Lazy.force g

let () =
  let () = print_endline "loop2" in
  let g = loop2 () in
  let _ = ignore @@ get_ep a g; print_endline "A ok"
  and _ = ignore @@ get_ep b g; print_endline "B ok"
  and _ = ignore @@ get_ep c g; print_endline "C ok"
  in
  ()
