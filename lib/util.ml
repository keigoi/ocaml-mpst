open Base

let a = {role_label={make_obj=(fun v->object method role_A=v end);
                     call_obj=(fun o->o#role_A)};
         role_index=Zero}
let b = {role_label={make_obj=(fun v->object method role_B=v end);
                     call_obj=(fun o->o#role_B)};
         role_index=Succ Zero}
let c = {role_label={make_obj=(fun v->object method role_C=v end);
                     call_obj=(fun o->o#role_C)};
         role_index=Succ (Succ Zero)}
let d = {role_label={make_obj=(fun v->object method role_D=v end);
                     call_obj=(fun o->o#role_D)};
         role_index=Succ (Succ (Succ Zero))}

let msg =
  {obj={make_obj=(fun f -> object method msg=f end);
        call_obj=(fun o -> o#msg)};
   var=(fun v -> `msg(v))}
let left =
  {obj={make_obj=(fun f -> object method left=f end);
        call_obj=(fun o -> o#left)};
   var=(fun v -> `left(v))}
let right =
  {obj={make_obj=(fun f -> object method right=f end);
        call_obj=(fun o -> o#right)};
   var=(fun v -> `right(v))}
let middle =
  {obj={make_obj=(fun f -> object method middle=f end);
        call_obj=(fun o -> o#middle)};
   var=(fun v -> `middle(v))}
let ping =
  {obj={make_obj=(fun f -> object method ping=f end);
        call_obj=(fun o -> o#ping)};
   var=(fun v -> `ping(v))}
let pong =
  {obj={make_obj=(fun f -> object method pong=f end);
        call_obj=(fun o -> o#pong)};
   var=(fun v -> `pong(v))}
let fini =
  {obj={make_obj=(fun f -> object method fini=f end);
        call_obj=(fun o -> o#fini)};
   var=(fun v -> `fini(v))}

let left_or_right =
  {disj_concat=(fun l r -> object method left=l#left method right=r#right end);
   disj_splitL=(fun lr -> (lr :> <left : _>));
   disj_splitR=(fun lr -> (lr :> <right : _>));
  }
let right_or_left =
  {disj_concat=(fun l r -> object method right=l#right method left=r#left end);
   disj_splitL=(fun lr -> (lr :> <right : _>));
   disj_splitR=(fun lr -> (lr :> <left : _>));
  }


let to_ m r1 r2 r3 =
  let (!) x = x.role_label in
  {disj_concat=(fun l r -> !r1.make_obj (m.disj_concat (!r2.call_obj l) (!r3.call_obj r)));
   disj_splitL=(fun lr -> !r2.make_obj (m.disj_splitL @@ !r1.call_obj lr));
   disj_splitR=(fun lr -> !r3.make_obj (m.disj_splitR @@ !r1.call_obj lr));
  }
let to_a m = to_ m a a a
let to_b m = to_ m b b b
let to_c m = to_ m c c c
let to_d m = to_ m d d d

let left_middle_or_right =
  {disj_concat=(fun l r -> object method left=l#left method middle=l#middle method right=r#right end);
   disj_splitL=(fun lr -> (lr :> <left : _; middle: _>));
   disj_splitR=(fun lr -> (lr :> <right : _>));
  }

let left_or_middle =
  {disj_concat=(fun l r -> object method left=l#left method middle=r#middle end);
   disj_splitL=(fun lr -> (lr :> <left : _>));
   disj_splitR=(fun lr -> (lr :> <middle : _>));
  }

let left_or_middle_right =
  {disj_concat=(fun l r -> object method left=l#left method middle=r#middle method right=r#right end);
   disj_splitL=(fun lr -> (lr :> <left : _>));
   disj_splitR=(fun lr -> (lr :> <middle: _; right : _>));
  }

let middle_or_right =
  {disj_concat=(fun l r -> object method middle=l#middle method right=r#right end);
   disj_splitL=(fun lr -> (lr :> <middle : _>));
   disj_splitR=(fun lr -> (lr :> <right : _>));
  }

