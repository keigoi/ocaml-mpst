open Mpst


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
               var=(fun v -> `login(v))}
let password = {obj={make_obj=(fun v-> object method password=v end);
                  call_obj=(fun o->o#password)};
               var=(fun v -> `password(v))}
let auth = {obj={make_obj=(fun v-> object method auth=v end);
                  call_obj=(fun o->o#auth)};
               var=(fun v -> `auth(v))}
let cancel = {obj={make_obj=(fun v-> object method cancel=v end);
                  call_obj=(fun o->o#cancel)};
               var=(fun v -> `cancel(v))}
let quit = {obj={make_obj=(fun v-> object method quit=v end);
                  call_obj=(fun o->o#quit)};
               var=(fun v -> `quit(v))}
let retry = {obj={make_obj=(fun v-> object method retry=v end);
                  call_obj=(fun o->o#retry)};
               var=(fun v -> `retry(v))}

let login_or_cancel =
  {disj_merge=(fun l r -> object method login=l#login method cancel=r#cancel end);
   disj_splitL=(fun lr -> (lr :> <login : _>));
   disj_splitR=(fun lr -> (lr :> <cancel : _>));
  }

let login_cancel_or_retry =
  {disj_merge=(fun l r -> object method login=l#login method cancel=l#cancel method retry=r#retry end);
   disj_splitL=(fun lr -> (lr :> <login : _; cancel : _>));
   disj_splitR=(fun lr -> (lr :> <retry : _>));
  }
