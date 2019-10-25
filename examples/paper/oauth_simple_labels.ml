open Mpst;;

(* label objects *)
let login =
  {obj={call_obj=(fun x->x#login); make_obj=(fun x->object method login=x end)};
   var=(fun v -> `login(v))}

let pwd =
  {obj={call_obj=(fun x->x#pwd); make_obj=(fun x->object method pwd=x end)};
   var=(fun v -> `pwd(v))}

let auth =
  {obj={call_obj=(fun x->x#auth); make_obj=(fun x->object method auth=x end)};
   var=(fun v -> `auth(v))}

let s =
  {role_index=Zero;
   role_label=
     {call_obj=(fun x->x#role_S); make_obj=(fun x->object method role_S=x end)}}

let c =
  {role_index=Succ Zero;
   role_label=
     {call_obj=(fun x->x#role_C); make_obj=(fun x->object method role_C=x end)}}

(* we can reuse Util.a, but anyway redefine it (albeit with a different index) *)
let a =
  {role_index=Succ (Succ Zero);
   role_label=
     {call_obj=(fun x->x#role_A); make_obj=(fun x->object method role_A=x end)}}

let to_s m = to_ m s s s
let to_c m = to_ m c c c
let to_a m = to_ m a a a


