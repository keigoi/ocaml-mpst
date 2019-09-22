open Mpst
open Toy_oauth_util

let () =
  let oauth = gen @@ (s --> c) login @@ (c --> a) password @@ (a --> c) auth finish
  in
  let chs, chc, cha = get_ch s oauth, get_ch c oauth, get_ch a oauth
  in
  let _thread_a () =
    let `password((p:string), cha) = receive cha#role_C in
    let cha = send cha#role_C#auth true in
    close cha
  in
  ()

(* let x = fix (fun x -> x) *)

let () =
  let oauth1 () =
      choice_at s (to_c login_or_cancel) 
        (s, (s --> c) login @@ (c --> a) password @@ (a --> c) auth finish)
        (s, (s --> c) cancel @@ (c --> a) quit finish)
  in
  let oauth1' = gen @@ oauth1 ()
  in
  let chs, chc, cha = get_ch s oauth1', get_ch c oauth1', get_ch a oauth1'
  in
  let _thread_c () =
    match receive chc#role_S with
    | `cancel((code:int), chc) -> close (send chc#role_A#quit ())
    | `login((), chc) -> 
       let chc = send chc#role_A#password "asdf" in
       let `auth(b, chc) = receive chc#role_A in
       close chc
  in
  let oauth2 =
    gen @@
      fix (fun x -> 
          choice_at s (to_c login_cancel_or_retry) 
            (s, oauth1 ())
            (s, (s --> c) retry @@ (c --> a) retry x))
  in
  let _chc, _chs = get_ch c oauth2, get_ch s oauth2
  in
  ()
