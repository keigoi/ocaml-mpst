open Mpst.Types

let p = {role_index=Zero;
         role_label={make_obj=(fun v->object method role_P=v end);
                call_obj=(fun o->o#role_P)}}
let r = {role_index=Succ Zero;
         role_label={make_obj=(fun v->object method role_R=v end);
                call_obj=(fun o->o#role_R)}}
let c = {role_index=Succ (Succ Zero);
         role_label={make_obj=(fun v->object method role_C=v end);
                call_obj=(fun o->o#role_C)}}

let plane = {obj={make_obj=(fun v-> object method plane=v end);
                  call_obj=(fun o->o#plane)};
             var={make_var=(fun v -> `plane(v));
                  match_var=(function `plane(v) -> Some v | _ -> None)}}
let is_above = {obj={make_obj=(fun v-> object method is_above=v end);
                     call_obj=(fun o->o#is_above)};
                var={make_var=(fun v -> `is_above(v));
                     match_var=(function `is_above(v) -> Some v | _ -> None)}}
let both_in = {obj={make_obj=(fun v-> object method both_in=v end);
                    call_obj=(fun o->o#both_in)};
               var={make_var=(fun v -> `both_in(v));
                    match_var=(function `both_in(v) -> Some v | _ -> None)}}
let both_out = {obj={make_obj=(fun v-> object method both_out=v end);
                     call_obj=(fun o->o#both_out)};
                var={make_var=(fun v -> `both_out(v));
                     match_var=(function `both_out(v) -> Some v | _ -> None)}}
let intersect = {obj={make_obj=(fun v-> object method intersect=v end);
                      call_obj=(fun o->o#intersect)};
                 var={make_var=(fun v -> `intersect(v));
                      match_var=(function `intersect(v) -> Some v | _ -> None)}}
let res = {obj={make_obj=(fun v-> object method res=v end);
                call_obj=(fun o->o#res)};
           var={make_var=(fun v -> `res(v));
                match_var=(function `res(v) -> Some v | _ -> None)}}
let secout = {obj={make_obj=(fun v-> object method secout=v end);
                   call_obj=(fun o->o#secout)};
              var={make_var=(fun v -> `secout(v));
                   match_var=(function `secout(v) -> Some v | _ -> None)}}
let secin = {obj={make_obj=(fun v-> object method secin=v end);
                  call_obj=(fun o->o#secin)};
             var={make_var=(fun v -> `secin(v));
                  match_var=(function `secin(v) -> Some v | _ -> None)}}
let close_ = {obj={make_obj=(fun v-> object method close=v end);
                   call_obj=(fun o->o#close)};
              var={make_var=(fun v -> `close(v));
                   match_var=(function `close(v) -> Some v | _ -> None)}}

let isabove_or_close =
  {disj_concat=(fun l r -> object method is_above=l#is_above method close=r#close end);
   disj_splitL=(fun lr -> (lr :> <is_above : _>));
   disj_splitR=(fun lr -> (lr :> <close : _>));
  }
let bothin_or_bothout =
  {disj_concat=(fun l r -> object method both_in=l#both_in method both_out=r#both_out end);
   disj_splitL=(fun lr -> (lr :> <both_in : _>));
   disj_splitR=(fun lr -> (lr :> <both_out : _>));
  }
let secout_or_secin =
  {disj_concat=(fun l r -> object method secout=l#secout method secin=r#secin end);
   disj_splitL=(fun lr -> (lr :> <secout : _>));
   disj_splitR=(fun lr -> (lr :> <secin : _>));
  }
let bothin_bothout_or_intersect =
  {disj_concat=(fun l r -> object method both_in=l#both_in method both_out=l#both_out method intersect=r#intersect end);
   disj_splitL=(fun lr -> (lr :> <both_in : _; both_out : _>));
   disj_splitR=(fun lr -> (lr :> <intersect : _>));
  }
let to_r m =
  {disj_concat=(fun l r -> object method role_R=m.disj_concat l#role_R r#role_R end);
   disj_splitL=(fun lr -> object method role_R=m.disj_splitL lr#role_R end);
   disj_splitR=(fun lr -> object method role_R=m.disj_splitR lr#role_R end);
  }
let to_c m =
  {disj_concat=(fun l r -> object method role_C=m.disj_concat l#role_C r#role_C end);
   disj_splitL=(fun lr -> object method role_C=m.disj_splitL lr#role_C end);
   disj_splitR=(fun lr -> object method role_C=m.disj_splitR lr#role_C end);
  }
