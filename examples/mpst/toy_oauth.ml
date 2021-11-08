(*
 examples from K. Imai, R. Neykova, N. Yoshida and S. Yuen,
 "Multiparty Session Programming with Global Protocol Combinators",
 2020.
*)

(* The libraries below are compulsory *)
open Concur_shims (* utilities for switching between Threads and lwt*)

open Mpst (* the mpst-ocaml library *)

open Toy_oauth_util (* includes the definitions of custom roles and labels *)

let ( let* ) = IO.bind

(*======PART I: A simple protocol: Corresponds to Figure 2======*)
let () =
  (*1. Write a global combinator *)
  let oauth =
    gen @@ (s --> c) login @@ (c --> a) password @@ (a --> c) auth finish
  in
  (* 2. Get channel vectors for each role *)
  let _chs, _chc, cha = (get_ch s oauth, get_ch c oauth, get_ch a oauth) in
  (* 3. Implement the processes using their respective channel vectors *)
  let _thread_a () =
    let* (`password ((_ : string), cha)) = receive cha#role_C in
    let* cha = send cha#role_C#auth true in
    close cha
  in
  ()

(*====PART II: A protocol with branching and recursion (Fig. 3a and Fig 3b)=====*)
let () =
  (* 1. Write a global combinator *)
  let oauth2 () =
    choice_at s (to_c login_or_cancel)
      (s, (s --> c) login @@ (c --> a) password @@ (a --> c) auth finish)
      (s, (s --> c) cancel @@ (c --> a) quit finish)
  in
  let oauth2' = gen @@ oauth2 () in
  (* 2. Get channel vectors for each role *)
  let _chs, chc, _cha =
    (get_ch s oauth2', get_ch c oauth2', get_ch a oauth2')
  in
  (* 3. Implement the processes using their respective channel vectors *)
  let _thread_c () =
    let* var = receive chc#role_S in
    match var with
    | `cancel ((_ : int), chc) ->
        let* chc = send chc#role_A#quit () in
        close chc
    | `login ((), chc) ->
        let* chc = send chc#role_A#password "asdf" in
        let* (`auth (_, chc)) = receive chc#role_A in
        close chc
  in
  let oauth3 =
    fix (fun x ->
        choice_at s
          (to_c login_cancel_or_retry)
          (s, oauth2 ())
          (s, (s --> c) retry @@ (c --> a) retry x))
  in
  let oauth3' = gen oauth3 in
  let _chc, _chs = (get_ch c oauth3', get_ch s oauth3') in

  (*=====PART III: Wrong protocol (Fig. 5)======*)
  (* Uncomment the lines below*)
  (* let _oauth4 =
       choice_at s (to_c login_cancel_or_retry)
         (s, oauth2 ())
        (s, (s --> a) cancel @@
            (c --> a) quit @@
            finish)
     in *)
  ()
