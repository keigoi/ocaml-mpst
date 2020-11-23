open Mpst.Types
open Mpst.Util


let f () = `A (*  [`A | ρ] *)
let g () = `B (*  [`B | ρ] *)

let h () = if true then f() else g() (*  [`A | `B | ρ] *)


let f () = if true then `A else `B


let s = {
   role_index=Zero;
           role_label={make_obj=(fun v->object method role_S=v end);
                       call_obj=(fun o->o#role_S)}}
let c = {role_index=Succ Zero;
           role_label={make_obj=(fun v->object method role_C=v end);
                       call_obj=(fun o->o#role_C)}}
let a = {role_index=Succ (Succ Zero);
           role_label={make_obj=(fun v->object method role_A=v end);
                       call_obj=(fun o->o#role_A)}}

let to_s m = to_ m s s s
let to_c m = to_ m c c c
let to_a m = to_ m a a a


let login = {obj={make_obj=(fun v-> object method login=v end);
                  call_obj=(fun o->o#login)};
               var={make_var=(fun v -> `login(v));
                    match_var=(function `login(v) -> Some v | _ -> None)}}
let password = {obj={make_obj=(fun v-> object method password=v end);
                  call_obj=(fun o->o#password)};
               var={make_var=(fun v -> `password(v));
                    match_var=(function `password(v) -> Some v | _ -> None)}}
let auth = {obj={make_obj=(fun v-> object method auth=v end);
                  call_obj=(fun o->o#auth)};
               var={make_var=(fun v -> `auth(v));
                    match_var=(function `auth(v) -> Some v | _ -> None)}}
let cancel = {obj={make_obj=(fun v-> object method cancel=v end);
                  call_obj=(fun o->o#cancel)};
               var={make_var=(fun v -> `cancel(v));
                    match_var=(function `cancel(v) -> Some v | _ -> None)}}
let quit = {obj={make_obj=(fun v-> object method quit=v end);
                  call_obj=(fun o->o#quit)};
               var={make_var=(fun v -> `quit(v));
                    match_var=(function `quit(v) -> Some v | _ -> None)}}
let retry = {obj={make_obj=(fun v-> object method retry=v end);
                  call_obj=(fun o->o#retry)};
               var={make_var=(fun v -> `retry(v));
                    match_var=(function `retry(v) -> Some v | _ -> None)}}

let login_or_cancel =
  {disj_concat=(fun l r -> object method login=l#login method cancel=r#cancel end);
   disj_splitL=(fun lr -> (lr :> <login : _>));
   disj_splitR=(fun lr -> (lr :> <cancel : _>));
  }

let login_cancel_or_retry =
  {disj_concat=(fun l r -> object method login=l#login method cancel=l#cancel method retry=r#retry end);
   disj_splitL=(fun lr -> (lr :> <login : _; cancel : _>));
   disj_splitR=(fun lr -> (lr :> <retry : _>));
  }
