open Global
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
  
let left_or_right =
  {obj_merge=(fun l r -> object method left=l#left method right=r#right end);
   obj_splitL=(fun lr -> (lr :> <left : _>));
   obj_splitR=(fun lr -> (lr :> <right : _>));
  }
let right_or_left =
  {obj_merge=(fun l r -> object method right=l#right method left=r#left end);
   obj_splitL=(fun lr -> (lr :> <right : _>));
   obj_splitR=(fun lr -> (lr :> <left : _>));
  }
let to_b m =
  {obj_merge=(fun l r -> object method role_B=m.obj_merge l#role_B r#role_B end);
   obj_splitL=(fun lr -> object method role_B=m.obj_splitL lr#role_B end);
   obj_splitR=(fun lr -> object method role_B=m.obj_splitR lr#role_B end);
  }
let b_or_c =
  {obj_merge=(fun l r -> object method role_B=l#role_B method role_C=r#role_C end);
   obj_splitL=(fun lr -> (lr :> <role_B : _>));
   obj_splitR=(fun lr -> (lr :> <role_C : _>));
  }

let left_middle_or_right =
  {obj_merge=(fun l r -> object method left=l#left method middle=l#middle method right=r#right end);
   obj_splitL=(fun lr -> (lr :> <left : _; middle: _>));
   obj_splitR=(fun lr -> (lr :> <right : _>));
  }

let left_or_middle =
  {obj_merge=(fun l r -> object method left=l#left method middle=r#middle end);
   obj_splitL=(fun lr -> (lr :> <left : _>));
   obj_splitR=(fun lr -> (lr :> <middle : _>));
  }

let left_or_middle_right =
  {obj_merge=(fun l r -> object method left=l#left method middle=r#middle method right=r#right end);
   obj_splitL=(fun lr -> (lr :> <left : _>));
   obj_splitR=(fun lr -> (lr :> <middle: _; right : _>));
  }

let middle_or_right =
  {obj_merge=(fun l r -> object method middle=l#middle method right=r#right end);
   obj_splitL=(fun lr -> (lr :> <middle : _>));
   obj_splitR=(fun lr -> (lr :> <right : _>));
  }
