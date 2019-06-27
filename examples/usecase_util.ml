open Mpst

let title =
  {obj={make_obj=(fun f -> object method title=f end);
        call_obj=(fun o -> o#title)};
   var=(fun v -> `title(v))}

let quote =
  {obj={make_obj=(fun f -> object method quote=f end);
        call_obj=(fun o -> o#quote)};
   var=(fun v -> `quote(v))}

let quote_by_two =
  {obj={make_obj=(fun f -> object method quote_by_two=f end);
        call_obj=(fun o -> o#quote_by_two)};
   var=(fun v -> `quote_by_two(v))}

let ok =
  {obj={make_obj=(fun f -> object method ok=f end);
        call_obj=(fun o -> o#ok)};
   var=(fun v -> `ok(v))}

let quit =
  {obj={make_obj=(fun f -> object method quit=f end);
        call_obj=(fun o -> o#quit)};
   var=(fun v -> `quit(v))}

let date =
  {obj={make_obj=(fun f -> object method date=f end);
        call_obj=(fun o -> o#date)};
   var=(fun v -> `date(v))}

let ok_or_quit =
  {obj_merge=(fun l r -> object method ok=l#ok method quit=r#quit end);
   obj_splitL=(fun lr -> (lr :> <ok : _>));
   obj_splitR=(fun lr -> (lr :> <quit : _>));
  }

let sum =
  {obj={make_obj=(fun f -> object method sum=f end);
        call_obj=(fun o -> o#sum)};
   var=(fun v -> `sum(v))}

let multiply =
  {obj={make_obj=(fun f -> object method multiply=f end);
        call_obj=(fun o -> o#multiply)};
   var=(fun v -> `multiply(v))}

let result =
  {obj={make_obj=(fun f -> object method result=f end);
        call_obj=(fun o -> o#result)};
   var=(fun v -> `result(v))}

let sum_or_multiply =
  {obj_merge=(fun l r -> object method sum=l#sum method multiply=r#multiply end);
   obj_splitL=(fun lr -> (lr :> <sum : _>));
   obj_splitR=(fun lr -> (lr :> <multiply : _>));
  }

let sum_multiply_or_quit =
  {obj_merge=(fun l r -> object method sum=l#sum method multiply=l#multiply method quit=r#quit end);
   obj_splitL=(fun lr -> (lr :> <sum : _; multiply: _>));
   obj_splitR=(fun lr -> (lr :> <quit : _>));
  }

let fibonacci =
  {obj={make_obj=(fun f -> object method fibonacci=f end);
        call_obj=(fun o -> o#fibonacci)};
   var=(fun v -> `fibonacci(v))}

let stop =
  {obj={make_obj=(fun f -> object method stop=f end);
        call_obj=(fun o -> o#stop)};
   var=(fun v -> `stop(v))}

let fibonacci_or_stop =
  {obj_merge=(fun l r -> object method fibonacci=l#fibonacci method stop=r#stop end);
   obj_splitL=(fun lr -> (lr :> <fibonacci : _>));
   obj_splitR=(fun lr -> (lr :> <stop : _>));
  }

let is_above =
  {obj={make_obj=(fun f -> object method is_above=f end);
        call_obj=(fun o -> o#is_above)};
   var=(fun v -> `is_above(v))}

let res =
  {obj={make_obj=(fun f -> object method res=f end);
        call_obj=(fun o -> o#res)};
   var=(fun v -> `res(v))}

let both_in =
  {obj={make_obj=(fun f -> object method both_in=f end);
        call_obj=(fun o -> o#both_in)};
   var=(fun v -> `both_in(v))}

let both_out =
  {obj={make_obj=(fun f -> object method both_out=f end);
        call_obj=(fun o -> o#both_out)};
   var=(fun v -> `both_out(v))}

let intersect =
  {obj={make_obj=(fun f -> object method intersect=f end);
        call_obj=(fun o -> o#intersect)};
   var=(fun v -> `intersect(v))}

let sec_in =
  {obj={make_obj=(fun f -> object method sec_in=f end);
        call_obj=(fun o -> o#sec_in)};
   var=(fun v -> `sec_in(v))}

let sec_out =
  {obj={make_obj=(fun f -> object method sec_out=f end);
        call_obj=(fun o -> o#sec_out)};
   var=(fun v -> `sec_out(v))}

let close_ =
  {obj={make_obj=(fun f -> object method close=f end);
        call_obj=(fun o -> o#close)};
   var=(fun v -> `close(v))}

let plane =
  {obj={make_obj=(fun f -> object method plane=f end);
        call_obj=(fun o -> o#plane)};
   var=(fun v -> `plane(v))}

let bothin_or_bothout =
  {obj_merge=(fun l r -> object method both_in=l#both_in method both_out=r#both_out end);
   obj_splitL=(fun lr -> (lr :> <both_in : _>));
   obj_splitR=(fun lr -> (lr :> <both_out : _>));
  }

let bothin_bothout_or_intersect =
  {obj_merge=(fun l r -> object method both_in=l#both_in method both_out=l#both_out method intersect=r#intersect end);
   obj_splitL=(fun lr -> (lr :> <both_in : _; both_out: _>));
   obj_splitR=(fun lr -> (lr :> <intersect : _>));
  }

let secout_or_secin =
  {obj_merge=(fun l r -> object method sec_out=l#sec_out method sec_in=r#sec_in end);
   obj_splitL=(fun lr -> (lr :> <sec_out : _>));
   obj_splitR=(fun lr -> (lr :> <sec_in : _>));
  }

let isabove_or_close =
  {obj_merge=(fun l r -> object method is_above=l#is_above method close=r#close end);
   obj_splitL=(fun lr -> (lr :> <is_above : _>));
   obj_splitR=(fun lr -> (lr :> <close : _>));
  }

let propose =
  {obj={make_obj=(fun f -> object method propose=f end);
        call_obj=(fun o -> o#propose)};
   var=(fun v -> `propose(v))}

let accpt =
  {obj={make_obj=(fun f -> object method accpt=f end);
        call_obj=(fun o -> o#accpt)};
   var=(fun v -> `acppt(v))}

let confirm =
  {obj={make_obj=(fun f -> object method confirm=f end);
        call_obj=(fun o -> o#confirm)};
   var=(fun v -> `confirm(v))}

let reject =
  {obj={make_obj=(fun f -> object method reject=f end);
        call_obj=(fun o -> o#reject)};
   var=(fun v -> `reject(v))}
  
let accpt_reject_or_propose =
  {obj_merge=(fun l r -> object method accpt=l#accpt method reject=l#reject method propose=r#propose end);
   obj_splitL=(fun lr -> (lr :> <accpt : _; reject: _>));
   obj_splitR=(fun lr -> (lr :> <propose : _>));
  }
  
let accpt_or_reject =
  {obj_merge=(fun l r -> object method accpt=l#accpt method reject=r#reject end);
   obj_splitL=(fun lr -> (lr :> <accpt : _>));
   obj_splitR=(fun lr -> (lr :> <reject : _>));
  }

let authenticate =
  {obj={make_obj=(fun f -> object method authenticate=f end);
        call_obj=(fun o -> o#authenticate)};
   var=(fun v -> `authenticate(v))}

let yes =
  {obj={make_obj=(fun f -> object method yes=f end);
        call_obj=(fun o -> o#yes)};
   var=(fun v -> `yes(v))}

let no =
  {obj={make_obj=(fun f -> object method no=f end);
        call_obj=(fun o -> o#no)};
   var=(fun v -> `no(v))}
  
let ok_or_reject =
  {obj_merge=(fun l r -> object method ok=l#ok method reject=r#reject end);
   obj_splitL=(fun lr -> (lr :> <ok : _>));
   obj_splitR=(fun lr -> (lr :> <reject : _>));
  }

let yes_or_no =
  {obj_merge=(fun l r -> object method yes=l#yes method no=r#no end);
   obj_splitL=(fun lr -> (lr :> <yes : _>));
   obj_splitR=(fun lr -> (lr :> <no : _>));
  }

let query =
  {obj={make_obj=(fun f -> object method query=f end);
        call_obj=(fun o -> o#query)};
   var=(fun v -> `query(v))}

let quote =
  {obj={make_obj=(fun f -> object method quote=f end);
        call_obj=(fun o -> o#quote)};
   var=(fun v -> `quote(v))}

let dummy =
  {obj={make_obj=(fun f -> object method dummy=f end);
        call_obj=(fun o -> o#dummy)};
   var=(fun v -> `dummy(v))}

let payment =
  {obj={make_obj=(fun f -> object method payment=f end);
        call_obj=(fun o -> o#payment)};
   var=(fun v -> `payment(v))}

let ack =
  {obj={make_obj=(fun f -> object method ack=f end);
        call_obj=(fun o -> o#ack)};
   var=(fun v -> `ack(v))}

let query_or_yes_no =
  {obj_merge=(fun l r -> object method query=l#query method yes=r#yes method no=r#no end);
   obj_splitL=(fun lr -> (lr :> <query : _>));
   obj_splitR=(fun lr -> (lr :> <yes : _; no : _>));
  }

let bye =
  {obj={make_obj=(fun f -> object method bye=f end);
        call_obj=(fun o -> o#bye)};
   var=(fun v -> `bye(v))}
