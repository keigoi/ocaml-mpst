open Mpst

module Make(EP:S.ENDPOINT) = struct
module G = Mpst.Global_common.Make(EP)
open G

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
  {disj_merge=(fun l r -> object method compute=l#compute method result=r#result end);
   disj_splitL=(fun lr -> (lr :> <compute : _>));
   disj_splitR=(fun lr -> (lr :> <result : _>));
  }
let result_or_compute =
  {disj_merge=(fun l r -> object method result=l#result method compute=r#compute end);
   disj_splitL=(fun lr -> (lr :> <result : _>));
   disj_splitR=(fun lr -> (lr :> <compute : _>));
  }
let to_srv m =
  {disj_merge=(fun l r -> object method role_Srv=m.disj_merge l#role_Srv r#role_Srv end);
   disj_splitL=(fun lr -> object method role_Srv=m.disj_splitL lr#role_Srv end);
   disj_splitR=(fun lr -> object method role_Srv=m.disj_splitR lr#role_Srv end);
  }
end
module Dyn = Make(Mpst.Dyncheck)
