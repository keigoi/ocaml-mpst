(* sending from a non-choosing role *)
open Mpst_monad
open Linocaml

let s = Linocaml.Zero
let g = Linocaml.(Succ Zero)

let roleenabling =
  choice_at a (to_b left_or_right)
    (a, (a --> b) left @@
        (c --> b) msg @@
        (b --> c) left @@ finish)
    (a, (a --> b) right @@
        (c --> b) msg @@
        (b --> c) right @@ finish)

let tA () =
  print_endline "A start";
  let%lin #s =
    if Random.bool () then
      s <@ send (fun x->x#role_B#left) ()
    else
      s <@ send (fun x->x#role_B#right) ()
  in
  print_endline "A done";
  s <@ close

let tB () =
  print_endline "B start";
  match%lin s <@ receive (fun x->x#role_A) with
  | `left((), #s) ->
     print_endline "B left";
     let%lin `msg((), #s) = s <@ receive (fun x->x#role_C) in
     let%lin #s = s <@ send (fun x->x#role_C#left) () in
     s <@ close
  | `right((), #s) ->
     print_endline "B right";
     let%lin `msg((), #s) = s <@ receive (fun x->x#role_C) in
     let%lin #s = s <@ send (fun x->x#role_C#right) () in
     s <@ close

let tC () =
  print_endline "C start";
  let%lin #s = s <@ send (fun x->x#role_B#msg) () in
  begin match%lin s <@ receive (fun x->x#role_B) with
  | `left((), #s) ->
     print_endline "C left";
     s <@ close
  | `right((), #s) ->
     print_endline "C right";
     s <@ close
  end >>= fun () ->
  print_endline "C done";
  return ()


let () =
  Random.self_init ();
  Linocaml.run' (fun () ->
      let%lin #g = gen roleenabling in
      let%lin #s = g <@ get_ep a in
      thread_create s tA () >>
      let%lin #s = g <@ get_ep b in
      thread_create s tB () >>
      let%lin #s = g <@ get_ep c in
      g <@ degen >>
      tC () >>= fun () ->
      return ()
    ) ()
let f m =
  let%lin `m() = m in
  return ()
