open Mpst

let cli = {role_index=Zero;
           role_label={make_obj=(fun v->object method role_Cli=v end);
                       call_obj=(fun o->o#role_Cli)}}
let srv = {role_index=Succ Zero;
           role_label={make_obj=(fun v->object method role_Srv=v end);
                       call_obj=(fun o->o#role_Srv)}}

let compute = {obj={make_obj=(fun v-> object method compute=v end);
                    call_obj=(fun o->o#compute)};
               var=(fun v -> `compute(v))}
let result = {obj={make_obj=(fun v-> object method result=v end);
                   call_obj=(fun o->o#result)};
              var=(fun v -> `result(v))}
let answer = {obj={make_obj=(fun v-> object method answer=v end);
                   call_obj=(fun o->o#answer)};
              var=(fun v -> `answer(v))}
let compute_or_result =
  {obj_merge=(fun l r -> object method compute=l#compute method result=r#result end);
   obj_splitL=(fun lr -> (lr :> <compute : _>));
   obj_splitR=(fun lr -> (lr :> <result : _>));
  }
let to_srv m =
  {obj_merge=(fun l r -> object method role_Srv=m.obj_merge l#role_Srv r#role_Srv end);
   obj_splitL=(fun lr -> object method role_Srv=m.obj_splitL lr#role_Srv end);
   obj_splitR=(fun lr -> object method role_Srv=m.obj_splitR lr#role_Srv end);
  }
