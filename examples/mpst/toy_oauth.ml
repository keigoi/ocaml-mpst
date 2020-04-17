(*
 examples from K. Imai, R. Neykova, N. Yoshida and S. Yuen,
 "Multiparty Session Programming with Global Protocol Combinators",
 2020.
*) 

open Concur_shims
open Mpst
open Toy_oauth_util

let (let*) = IO.bind

let () =
  let oauth = gen @@ (s --> c) login @@ (c --> a) password @@ (a --> c) auth finish
  in
  let _chs, _chc, cha = get_ch s oauth, get_ch c oauth, get_ch a oauth
  in
  let _thread_a () =
    let* `password((_:string), cha) = receive cha#role_C in
    let* cha = send cha#role_C#auth true in
    close cha
  in
  ()

(* let x = fix (fun x -> x) *)

let () =
  let oauth2 () =
      choice_at s (to_c login_or_cancel) 
        (s, (s --> c) login @@ (c --> a) password @@ (a --> c) auth finish)
        (s, (s --> c) cancel @@ (c --> a) quit finish)
  in
  let oauth2' = gen @@ oauth2 ()
  in
  let _chs, chc, _cha = get_ch s oauth2', get_ch c oauth2', get_ch a oauth2'
  in
  let _thread_c () =
    let* var = receive chc#role_S in
    match var with
    | `cancel((_:int), chc) ->
      let* chc = send chc#role_A#quit () in
      close chc
    | `login((), chc) -> 
       let* chc = send chc#role_A#password "asdf" in
       let* `auth(_, chc) = receive chc#role_A in
       close chc
  in
  let oauth3 =
      fix (fun x -> 
          choice_at s (to_c login_cancel_or_retry) 
            (s, oauth2 ())
            (s, (s --> c) retry @@ (c --> a) retry x))
  in
  let oauth3' = gen oauth3 in
  let _chc, _chs = get_ch c oauth3', get_ch s oauth3'
  in
  (* let _oauth4 =
   *   choice_at s (to_c login_cancel_or_retry)
   *     (s, oauth2 ())
   *     (s, (s --> a) cancel @@
   *         (c --> a) quit @@
   *         finish)
   * in *)
  ()
