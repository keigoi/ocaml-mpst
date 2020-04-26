open Mpst
open Mpst.Types
open Mpst.Util
open Usecase_util

  let s = {role_index=Zero; role_label={make_obj=(fun v -> object method role_S=v end); call_obj=(fun o->o#role_S)}}
  let a = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_A=v end); call_obj=(fun o->o#role_A)}}
  let c = {role_index=Succ (Succ Zero); role_label={make_obj=(fun v -> object method role_C=v end); call_obj=(fun o->o#role_C)}}

  let to_s m = to_ m s s s
  let to_a m = to_ m a a a
  let to_c m = to_ m c c c

  let oAuth3 () =
    choice_at s (to_c login_or_cancel) (* full merging at a: password or quit *)
      (s, (s --> c) login @@
          fix @@ fun t ->
            (c --> a) password @@
            choice_at a (to_s auth_or_again)
            (a, (a --> s) auth @@
                (s --> c) auth @@
                finish)
            (a, (a --> s) again @@
                (s --> c) again @@
                t))
      (s, (s --> c) cancel @@
          (c --> a) quit @@
          finish)
