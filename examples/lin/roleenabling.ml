(* sending from a non-choosing role *)
open Mpst_monad
open Linocaml

let s = Linocaml.Zero

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
  | `left({data=()}, #s) ->
     print_endline "B left";
     let%lin `msg({data=()}, #s) = s <@ receive (fun x->x#role_C) in
     let%lin #s = s <@ send (fun x->x#role_C#left) () in
     s <@ close
  | `right({data=()}, #s) ->
     print_endline "B right";
     let%lin `msg({data=()}, #s) = s <@ receive (fun x->x#role_C) in
     let%lin #s = s <@ send (fun x->x#role_C#right) () in
     s <@ close

let tC () =
  print_endline "C start";
  let%lin #s = s <@ send (fun x->x#role_B#msg) () in
  begin match%lin s <@ receive (fun x->x#role_B) with
  | `left({data=()}, #s) ->
     print_endline "C left";
     s <@ close
  | `right({data=()}, #s) ->
     print_endline "C right";
     s <@ close
  end >>= fun () ->
  print_endline "C done";
  return ()

let g = Linocaml.(Succ s)
let s1 = Linocaml.(Succ g)
let s2 = Linocaml.(Succ s1)

let () =
  Random.self_init ();
  Linocaml.run' (fun () ->
      let%lin #g = gen roleenabling in
      let%lin #g,#s1 = get_ch a @> g in
      let%lin #g,#s2 = get_ch b @> g in
      thread_create s1 tA () >>
      thread_create s2 tB () >>
      let%lin #g,#s = get_ch c @> g in
      tC () >>= fun () ->
      degen @> g >>
      return ()
    ) ()
let f m =
  let%lin `m() = m in
  return ()
