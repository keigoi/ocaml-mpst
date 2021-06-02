open Mpst.Types
open Mpst.Util

let p = {role_index=Zero; role_label={make_obj=(fun v -> object method role_P=v end); call_obj=(fun o->o#role_P)}}
let r = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_R=v end); call_obj=(fun o->o#role_R)}}
let s = {c with role_label={make_obj=(fun v->object method role_S=v end); call_obj=(fun o->o#role_S)}}
let v = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_V=v end); call_obj=(fun o->o#role_V)}}

let loginsvc     = {role_index=Zero; role_label={make_obj=(fun v -> object method role_Login=v end); call_obj=(fun o->o#role_Login)}}
let requestor    = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_Request=v end); call_obj=(fun o->o#role_Request)}}
let authorisesvc = {role_index=Succ (Succ Zero); role_label={make_obj=(fun v -> object method role_Auth=v end); call_obj=(fun o->o#role_Auth)}}
let filtersvc    = {role_index=Succ (Succ (Succ Zero)); role_label={make_obj=(fun v -> object method role_Filter=v end); call_obj=(fun o->o#role_Filter)}}
let suppliersvc  = {role_index=Succ (Succ (Succ (Succ Zero))); role_label={make_obj=(fun v -> object method role_Supply=v end); call_obj=(fun o->o#role_Supply)}}
let contractsvc  = {role_index=Succ (Succ (Succ (Succ (Succ Zero)))); role_label={make_obj=(fun v -> object method role_Contract=v end); call_obj=(fun o->o#role_Contract)}}
let customer = {role_index=Zero; role_label={make_obj=(fun v -> object method role_Customer=v end); call_obj=(fun o->o#role_Customer)}}
let shop = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_Shop=v end); call_obj=(fun o->o#role_Shop)}}
let barber = {role_index=Succ (Succ Zero); role_label={make_obj=(fun v -> object method role_Barber=v end); call_obj=(fun o->o#role_Barber)}}
let srv = {role_index=Zero; role_label={make_obj=(fun v -> object method role_Srv=v end); call_obj=(fun o->o#role_Srv)}}
let cli = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_Cli=v end); call_obj=(fun o->o#role_Cli)}}
let mst = {role_index=Zero; role_label={make_obj=(fun v -> object method role_Mst=v end); call_obj=(fun o->o#role_Mst)}}
let wrk = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_Wrk=v end); call_obj=(fun o->o#role_Wrk)}}
let santa = {role_index=Zero; role_label={make_obj=(fun v -> object method role_Santa=v end); call_obj=(fun o->o#role_Santa)}}
let reindeer = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_Reindeer=v end); call_obj=(fun o->o#role_Reindeer)}}
let elf = {role_index=Succ (Succ Zero); role_label={make_obj=(fun v -> object method role_Elf=v end); call_obj=(fun o->o#role_Elf)}}
                 
let to_p m = to_ m p p p
let to_r m = to_ m r r r
let to_s m = to_ m s s s
let to_v m = to_ m v v v
let to_loginsvc m = to_ m loginsvc loginsvc loginsvc
let to_requestor m = to_ m requestor requestor requestor
let to_authorisesvc m = to_ m authorisesvc authorisesvc authorisesvc
let to_filtersvc m = to_ m filtersvc filtersvc filtersvc
let to_suppliersvc m = to_ m suppliersvc suppliersvc suppliersvc
let to_contractsvc m = to_ m contractsvc contractsvc contractsvc
let to_customer m = to_ m customer customer customer
let to_shop m = to_ m shop shop shop
let to_barber m = to_ m barber barber barber
let to_srv m = to_ m srv srv srv
let to_cli m = to_ m cli cli cli
let to_mst m = to_ m mst mst mst
let to_wrk m = to_ m wrk wrk wrk
let to_santa m = to_ m santa santa santa
let to_reindeer m = to_ m reindeer reindeer reindeer
let to_elf m = to_ m elf elf elf

let title =
  {obj={make_obj=(fun f -> object method title=f end);
        call_obj=(fun o -> o#title)};
   var={make_var=(fun v -> `title(v)); match_var=(function `title(v) -> Some v | _ -> None)}}

let quote =
  {obj={make_obj=(fun f -> object method quote=f end);
        call_obj=(fun o -> o#quote)};
   var={make_var=(fun v -> `quote(v)); match_var=(function `quote(v) -> Some v | _ -> None)}}

let quote_by_two =
  {obj={make_obj=(fun f -> object method quote_by_two=f end);
        call_obj=(fun o -> o#quote_by_two)};
   var={make_var=(fun v -> `quote_by_two(v)); match_var=(function `quote_by_two(v) -> Some v | _ -> None)}}

let ok =
  {obj={make_obj=(fun f -> object method ok=f end);
        call_obj=(fun o -> o#ok)};
   var={make_var=(fun v -> `ok(v)); match_var=(function `ok(v) -> Some v | _ -> None)}}

let quit =
  {obj={make_obj=(fun f -> object method quit=f end);
        call_obj=(fun o -> o#quit)};
   var={make_var=(fun v -> `quit(v)); match_var=(function `quit(v) -> Some v | _ -> None)}}

let date =
  {obj={make_obj=(fun f -> object method date=f end);
        call_obj=(fun o -> o#date)};
   var={make_var=(fun v -> `date(v)); match_var=(function `date(v) -> Some v | _ -> None)}}

let ok_or_quit =
  {disj_concat=(fun l r -> object method ok=l#ok method quit=r#quit end);
   disj_splitL=(fun lr -> (lr :> <ok : _>));
   disj_splitR=(fun lr -> (lr :> <quit : _>));
  }

let sum =
  {obj={make_obj=(fun f -> object method sum=f end);
        call_obj=(fun o -> o#sum)};
   var={make_var=(fun v -> `sum(v)); match_var=(function `sum(v) -> Some v | _ -> None)}}

let multiply =
  {obj={make_obj=(fun f -> object method multiply=f end);
        call_obj=(fun o -> o#multiply)};
   var={make_var=(fun v -> `multiply(v)); match_var=(function `multiply(v) -> Some v | _ -> None)}}

let result =
  {obj={make_obj=(fun f -> object method result=f end);
        call_obj=(fun o -> o#result)};
   var={make_var=(fun v -> `result(v)); match_var=(function `result(v) -> Some v | _ -> None)}}

let sum_or_multiply =
  {disj_concat=(fun l r -> object method sum=l#sum method multiply=r#multiply end);
   disj_splitL=(fun lr -> (lr :> <sum : _>));
   disj_splitR=(fun lr -> (lr :> <multiply : _>));
  }

let sum_multiply_or_quit =
  {disj_concat=(fun l r -> object method sum=l#sum method multiply=l#multiply method quit=r#quit end);
   disj_splitL=(fun lr -> (lr :> <sum : _; multiply: _>));
   disj_splitR=(fun lr -> (lr :> <quit : _>));
  }

let fibonacci =
  {obj={make_obj=(fun f -> object method fibonacci=f end);
        call_obj=(fun o -> o#fibonacci)};
   var={make_var=(fun v -> `fibonacci(v)); match_var=(function `fibonacci(v) -> Some v | _ -> None)}}

let stop =
  {obj={make_obj=(fun f -> object method stop=f end);
        call_obj=(fun o -> o#stop)};
   var={make_var=(fun v -> `stop(v)); match_var=(function `stop(v) -> Some v | _ -> None)}}

let fibonacci_or_stop =
  {disj_concat=(fun l r -> object method fibonacci=l#fibonacci method stop=r#stop end);
   disj_splitL=(fun lr -> (lr :> <fibonacci : _>));
   disj_splitR=(fun lr -> (lr :> <stop : _>));
  }

let is_above =
  {obj={make_obj=(fun f -> object method is_above=f end);
        call_obj=(fun o -> o#is_above)};
   var={make_var=(fun v -> `is_above(v)); match_var=(function `is_above(v) -> Some v | _ -> None)}}

let res =
  {obj={make_obj=(fun f -> object method res=f end);
        call_obj=(fun o -> o#res)};
   var={make_var=(fun v -> `res(v)); match_var=(function `res(v) -> Some v | _ -> None)}}

let both_in =
  {obj={make_obj=(fun f -> object method both_in=f end);
        call_obj=(fun o -> o#both_in)};
   var={make_var=(fun v -> `both_in(v)); match_var=(function `both_in(v) -> Some v | _ -> None)}}

let both_out =
  {obj={make_obj=(fun f -> object method both_out=f end);
        call_obj=(fun o -> o#both_out)};
   var={make_var=(fun v -> `both_out(v)); match_var=(function `both_out(v) -> Some v | _ -> None)}}

let intersect =
  {obj={make_obj=(fun f -> object method intersect=f end);
        call_obj=(fun o -> o#intersect)};
   var={make_var=(fun v -> `intersect(v)); match_var=(function `intersect(v) -> Some v | _ -> None)}}

let sec_in =
  {obj={make_obj=(fun f -> object method sec_in=f end);
        call_obj=(fun o -> o#sec_in)};
   var={make_var=(fun v -> `sec_in(v)); match_var=(function `sec_in(v) -> Some v | _ -> None)}}

let sec_out =
  {obj={make_obj=(fun f -> object method sec_out=f end);
        call_obj=(fun o -> o#sec_out)};
   var={make_var=(fun v -> `sec_out(v)); match_var=(function `sec_out(v) -> Some v | _ -> None)}}

let close_ =
  {obj={make_obj=(fun f -> object method close=f end);
        call_obj=(fun o -> o#close)};
   var={make_var=(fun v -> `close(v)); match_var=(function `close(v) -> Some v | _ -> None)}}

let plane =
  {obj={make_obj=(fun f -> object method plane=f end);
        call_obj=(fun o -> o#plane)};
   var={make_var=(fun v -> `plane(v)); match_var=(function `plane(v) -> Some v | _ -> None)}}

let bothin_or_bothout =
  {disj_concat=(fun l r -> object method both_in=l#both_in method both_out=r#both_out end);
   disj_splitL=(fun lr -> (lr :> <both_in : _>));
   disj_splitR=(fun lr -> (lr :> <both_out : _>));
  }

let bothin_bothout_or_intersect =
  {disj_concat=(fun l r -> object method both_in=l#both_in method both_out=l#both_out method intersect=r#intersect end);
   disj_splitL=(fun lr -> (lr :> <both_in : _; both_out: _>));
   disj_splitR=(fun lr -> (lr :> <intersect : _>));
  }

let secout_or_secin =
  {disj_concat=(fun l r -> object method sec_out=l#sec_out method sec_in=r#sec_in end);
   disj_splitL=(fun lr -> (lr :> <sec_out : _>));
   disj_splitR=(fun lr -> (lr :> <sec_in : _>));
  }

let isabove_or_close =
  {disj_concat=(fun l r -> object method is_above=l#is_above method close=r#close end);
   disj_splitL=(fun lr -> (lr :> <is_above : _>));
   disj_splitR=(fun lr -> (lr :> <close : _>));
  }

let propose =
  {obj={make_obj=(fun f -> object method propose=f end);
        call_obj=(fun o -> o#propose)};
   var={make_var=(fun v -> `propose(v)); match_var=(function `propose(v) -> Some v | _ -> None)}}

let accpt =
  {obj={make_obj=(fun f -> object method accpt=f end);
        call_obj=(fun o -> o#accpt)};
   var={make_var=(fun v -> `acppt(v)); match_var=(function `acppt(v) -> Some v | _ -> None)}}

let confirm =
  {obj={make_obj=(fun f -> object method confirm=f end);
        call_obj=(fun o -> o#confirm)};
   var={make_var=(fun v -> `confirm(v)); match_var=(function `confirm(v) -> Some v | _ -> None)}}

let reject =
  {obj={make_obj=(fun f -> object method reject=f end);
        call_obj=(fun o -> o#reject)};
   var={make_var=(fun v -> `reject(v)); match_var=(function `reject(v) -> Some v | _ -> None)}}

let accpt_reject_or_propose =
  {disj_concat=(fun l r -> object method accpt=l#accpt method reject=l#reject method propose=r#propose end);
   disj_splitL=(fun lr -> (lr :> <accpt : _; reject: _>));
   disj_splitR=(fun lr -> (lr :> <propose : _>));
  }

let accpt_or_reject =
  {disj_concat=(fun l r -> object method accpt=l#accpt method reject=r#reject end);
   disj_splitL=(fun lr -> (lr :> <accpt : _>));
   disj_splitR=(fun lr -> (lr :> <reject : _>));
  }

let authenticate =
  {obj={make_obj=(fun f -> object method authenticate=f end);
        call_obj=(fun o -> o#authenticate)};
   var={make_var=(fun v -> `authenticate(v)); match_var=(function `authenticate(v) -> Some v | _ -> None)}}

let yes =
  {obj={make_obj=(fun f -> object method yes=f end);
        call_obj=(fun o -> o#yes)};
   var={make_var=(fun v -> `yes(v)); match_var=(function `yes(v) -> Some v | _ -> None)}}

let no =
  {obj={make_obj=(fun f -> object method no=f end);
        call_obj=(fun o -> o#no)};
   var={make_var=(fun v -> `no(v)); match_var=(function `no(v) -> Some v | _ -> None)}}

let ok_or_reject =
  {disj_concat=(fun l r -> object method ok=l#ok method reject=r#reject end);
   disj_splitL=(fun lr -> (lr :> <ok : _>));
   disj_splitR=(fun lr -> (lr :> <reject : _>));
  }

let yes_or_no =
  {disj_concat=(fun l r -> object method yes=l#yes method no=r#no end);
   disj_splitL=(fun lr -> (lr :> <yes : _>));
   disj_splitR=(fun lr -> (lr :> <no : _>));
  }

let query =
  {obj={make_obj=(fun f -> object method query=f end);
        call_obj=(fun o -> o#query)};
   var={make_var=(fun v -> `query(v)); match_var=(function `query(v) -> Some v | _ -> None)}}

let dummy =
  {obj={make_obj=(fun f -> object method dummy=f end);
        call_obj=(fun o -> o#dummy)};
   var={make_var=(fun v -> `dummy(v)); match_var=(function `dummy(v) -> Some v | _ -> None)}}

let payment =
  {obj={make_obj=(fun f -> object method payment=f end);
        call_obj=(fun o -> o#payment)};
   var={make_var=(fun v -> `payment(v)); match_var=(function `payment(v) -> Some v | _ -> None)}}

let ack =
  {obj={make_obj=(fun f -> object method ack=f end);
        call_obj=(fun o -> o#ack)};
   var={make_var=(fun v -> `ack(v)); match_var=(function `ack(v) -> Some v | _ -> None)}}

let query_or_yes_no =
  {disj_concat=(fun l r -> object method query=l#query method yes=r#yes method no=r#no end);
   disj_splitL=(fun lr -> (lr :> <query : _>));
   disj_splitR=(fun lr -> (lr :> <yes : _; no : _>));
  }

let bye =
  {obj={make_obj=(fun f -> object method bye=f end);
        call_obj=(fun o -> o#bye)};
   var={make_var=(fun v -> `bye(v)); match_var=(function `bye(v) -> Some v | _ -> None)}}

let login =
  {obj={make_obj=(fun f -> object method login=f end);
        call_obj=(fun o -> o#login)};
   var={make_var=(fun v -> `login(v)); match_var=(function `login(v) -> Some v | _ -> None)}}

let loginfailure =
  {obj={make_obj=(fun f -> object method loginfailure=f end);
        call_obj=(fun o -> o#loginfailure)};
   var={make_var=(fun v -> `loginfailure(v)); match_var=(function `loginfailure(v) -> Some v | _ -> None)}}

let loginsuccess =
  {obj={make_obj=(fun f -> object method loginsuccess=f end);
        call_obj=(fun o -> o#loginsuccess)};
   var={make_var=(fun v -> `loginsuccess(v)); match_var=(function `loginsuccess(v) -> Some v | _ -> None)}}

let getsuppliers =
  {obj={make_obj=(fun f -> object method getsuppliers=f end);
        call_obj=(fun o -> o#getsuppliers)};
   var={make_var=(fun v -> `getsuppliers(v)); match_var=(function `getsuppliers(v) -> Some v | _ -> None)}}

let getcontracts =
  {obj={make_obj=(fun f -> object method getcontracts=f end);
        call_obj=(fun o -> o#getcontracts)};
   var={make_var=(fun v -> `getcontracts(v)); match_var=(function `getcontracts(v) -> Some v | _ -> None)}}

let deny =
  {obj={make_obj=(fun f -> object method deny=f end);
        call_obj=(fun o -> o#deny)};
   var={make_var=(fun v -> `deny(v)); match_var=(function `deny(v) -> Some v | _ -> None)}}

let suppliers =
  {obj={make_obj=(fun f -> object method suppliers=f end);
        call_obj=(fun o -> o#suppliers)};
   var={make_var=(fun v -> `suppliers(v)); match_var=(function `suppliers(v) -> Some v | _ -> None)}}

let contracts =
  {obj={make_obj=(fun f -> object method contracts=f end);
        call_obj=(fun o -> o#contracts)};
   var={make_var=(fun v -> `contracts(v)); match_var=(function `contracts(v) -> Some v | _ -> None)}}

let filtered =
  {obj={make_obj=(fun f -> object method filtered=f end);
        call_obj=(fun o -> o#filtered)};
   var={make_var=(fun v -> `filtered(v)); match_var=(function `filtered(v) -> Some v | _ -> None)}}

let getsuppliers_or_getcontracts =
  {disj_concat=(fun l r -> object method getsuppliers=l#getsuppliers method getcontracts=r#getcontracts end);
   disj_splitL=(fun lr -> (lr :> <getsuppliers : _>));
   disj_splitR=(fun lr -> (lr :> <getcontracts : _>));
  }

let loginfailure_or_loginsuccess =
  {disj_concat=(fun l r -> object method loginfailure=l#loginfailure method loginsuccess=r#loginsuccess end);
   disj_splitL=(fun lr -> (lr :> <loginfailure : _>));
   disj_splitR=(fun lr -> (lr :> <loginsuccess : _>));
  }

let ehlo =
  {obj={make_obj=(fun f -> object method ehlo=f end);
        call_obj=(fun o -> o#ehlo)};
   var={make_var=(fun v -> `ehlo(v)); match_var=(function `ehlo(v) -> Some v | _ -> None)}}

let starttls =
  {obj={make_obj=(fun f -> object method starttls=f end);
        call_obj=(fun o -> o#starttls)};
   var={make_var=(fun v -> `starttls(v)); match_var=(function `starttls(v) -> Some v | _ -> None)}}

let auth =
  {obj={make_obj=(fun f -> object method auth=f end);
        call_obj=(fun o -> o#auth)};
   var={make_var=(fun v -> `auth(v)); match_var=(function `auth(v) -> Some v | _ -> None)}}

let mail =
  {obj={make_obj=(fun f -> object method mail=f end);
        call_obj=(fun o -> o#mail)};
   var={make_var=(fun v -> `mail(v)); match_var=(function `mail(v) -> Some v | _ -> None)}}

let rcpt =
  {obj={make_obj=(fun f -> object method rcpt=f end);
        call_obj=(fun o -> o#rcpt)};
   var={make_var=(fun v -> `rcpt(v)); match_var=(function `rcpt(v) -> Some v | _ -> None)}}

let data =
  {obj={make_obj=(fun f -> object method data=f end);
        call_obj=(fun o -> o#data)};
   var={make_var=(fun v -> `data(v)); match_var=(function `data(v) -> Some v | _ -> None)}}

let _220 =
  {obj={make_obj=(fun f -> object method _220=f end);
        call_obj=(fun o -> o#_220)};
   var={make_var=(fun v -> `_220(v)); match_var=(function `_220(v) -> Some v | _ -> None)}}

let _235 =
  {obj={make_obj=(fun f -> object method _235=f end);
        call_obj=(fun o -> o#_235)};
   var={make_var=(fun v -> `_235(v)); match_var=(function `_235(v) -> Some v | _ -> None)}}

let _250 =
  {obj={make_obj=(fun f -> object method _250=f end);
        call_obj=(fun o -> o#_250)};
   var={make_var=(fun v -> `_250(v)); match_var=(function `_250(v) -> Some v | _ -> None)}}

let _250d =
  {obj={make_obj=(fun f -> object method _250d=f end);
        call_obj=(fun o -> o#_250d)};
   var={make_var=(fun v -> `_250d(v)); match_var=(function `_250d(v) -> Some v | _ -> None)}}

let _354 =
  {obj={make_obj=(fun f -> object method _354=f end);
        call_obj=(fun o -> o#_354)};
   var={make_var=(fun v -> `_354(v)); match_var=(function `_354(v) -> Some v | _ -> None)}}

let _501 =
  {obj={make_obj=(fun f -> object method _501=f end);
        call_obj=(fun o -> o#_501)};
   var={make_var=(fun v -> `_501(v)); match_var=(function `_501(v) -> Some v | _ -> None)}}

let _535 =
  {obj={make_obj=(fun f -> object method _535=f end);
        call_obj=(fun o -> o#_535)};
   var={make_var=(fun v -> `_535(v)); match_var=(function `_535(v) -> Some v | _ -> None)}}

let mailbody =
  {obj={make_obj=(fun f -> object method mailbody=f end);
        call_obj=(fun o -> o#mailbody)};
   var={make_var=(fun v -> `mailbody(v)); match_var=(function `mailbody(v) -> Some v | _ -> None)}}

let starttls_or_quit =
  {disj_concat=(fun l r -> object method starttls=l#starttls method quit=r#quit end);
   disj_splitL=(fun lr -> (lr :> <starttls : _>));
   disj_splitR=(fun lr -> (lr :> <quit : _>));
  }

let ehlo_or_quit =
  {disj_concat=(fun l r -> object method ehlo=l#ehlo method quit=r#quit end);
   disj_splitL=(fun lr -> (lr :> <ehlo : _>));
   disj_splitR=(fun lr -> (lr :> <quit : _>));
  }

let mail_or_quit =
  {disj_concat=(fun l r -> object method mail=l#mail method quit=r#quit end);
   disj_splitL=(fun lr -> (lr :> <mail : _>));
   disj_splitR=(fun lr -> (lr :> <quit : _>));
  }

let _501_or_250 =
  {disj_concat=(fun l r -> object method _501=l#_501 method _250=r#_250 end);
   disj_splitL=(fun lr -> (lr :> <_501 : _>));
   disj_splitR=(fun lr -> (lr :> <_250 : _>));
  }

let rcpt_or_data =
  {disj_concat=(fun l r -> object method rcpt=l#rcpt method data=r#data end);
   disj_splitL=(fun lr -> (lr :> <rcpt : _>));
   disj_splitR=(fun lr -> (lr :> <data : _>));
  }

let auth_or_quit =
  {disj_concat=(fun l r -> object method auth=l#auth method quit=r#quit end);
   disj_splitL=(fun lr -> (lr :> <auth : _>));
   disj_splitR=(fun lr -> (lr :> <quit : _>));
  }

let _235_or_535 =
  {disj_concat=(fun l r -> object method _235=l#_235 method _535=r#_535 end);
   disj_splitL=(fun lr -> (lr :> <_235 : _>));
   disj_splitR=(fun lr -> (lr :> <_535 : _>));
  }

let _250d_or_250 =
  {disj_concat=(fun l r -> object method _250d=l#_250d method _250=r#_250 end);
   disj_splitL=(fun lr -> (lr :> <_250d : _>));
   disj_splitR=(fun lr -> (lr :> <_250 : _>));
  }

let _250d_250_or_220 =
  {disj_concat=(fun l r -> object method _250d=l#_250d method _250=l#_250 method _220=r#_220 end);
   disj_splitL=(fun lr -> (lr :> <_250d : _; _250 : _>));
   disj_splitR=(fun lr -> (lr :> <_220 : _>));
  }

let full =
  {obj={make_obj=(fun f -> object method full=f end);
        call_obj=(fun o -> o#full)};
   var={make_var=(fun v -> `full(v)); match_var=(function `full(v) -> Some v | _ -> None)}}

let seat =
  {obj={make_obj=(fun f -> object method seat=f end);
        call_obj=(fun o -> o#seat)};
   var={make_var=(fun v -> `seat(v)); match_var=(function `seat(v) -> Some v | _ -> None)}}

let available =
  {obj={make_obj=(fun f -> object method available=f end);
        call_obj=(fun o -> o#available)};
   var={make_var=(fun v -> `available(v)); match_var=(function `available(v) -> Some v | _ -> None)}}

let ready =
  {obj={make_obj=(fun f -> object method ready=f end);
        call_obj=(fun o -> o#ready)};
   var={make_var=(fun v -> `ready(v)); match_var=(function `ready(v) -> Some v | _ -> None)}}

let descr =
  {obj={make_obj=(fun f -> object method descr=f end);
        call_obj=(fun o -> o#descr)};
   var={make_var=(fun v -> `descr(v)); match_var=(function `descr(v) -> Some v | _ -> None)}}

let haircut =
  {obj={make_obj=(fun f -> object method haircut=f end);
        call_obj=(fun o -> o#haircut)};
   var={make_var=(fun v -> `haircut(v)); match_var=(function `haircut(v) -> Some v | _ -> None)}}

let pay =
  {obj={make_obj=(fun f -> object method pay=f end);
        call_obj=(fun o -> o#pay)};
   var={make_var=(fun v -> `pay(v)); match_var=(function `pay(v) -> Some v | _ -> None)}}

let full_or_seat =
  {disj_concat=(fun l r -> object method full=l#full method seat=r#seat end);
   disj_splitL=(fun lr -> (lr :> <full : _>));
   disj_splitR=(fun lr -> (lr :> <seat : _>));
  }

let deleg_customer =
  {obj={make_obj=(fun f -> object method deleg_customer=f end);
        call_obj=(fun o -> o#deleg_customer)};
   var={make_var=(fun v -> `deleg_customer(v)); match_var=(function `deleg_customer(v) -> Some v | _ -> None)}}

let start_smoking =
  {obj={make_obj=(fun f -> object method start_smoking=f end);
        call_obj=(fun o -> o#start_smoking)};
   var={make_var=(fun v -> `start_smoking(v)); match_var=(function `start_smoking(v) -> Some v | _ -> None)}}

let exit =
  {obj={make_obj=(fun f -> object method exit=f end);
        call_obj=(fun o -> o#exit)};
   var={make_var=(fun v -> `exit(v)); match_var=(function `exit(v) -> Some v | _ -> None)}}

let started_smoking =
  {obj={make_obj=(fun f -> object method started_smoking=f end);
        call_obj=(fun o -> o#started_smoking)};
   var={make_var=(fun v -> `started_smoking(v)); match_var=(function `started_smoking(v) -> Some v | _ -> None)}}

let start_smoking_or_exit =
  {disj_concat=(fun l r -> object method start_smoking=l#start_smoking method exit=r#exit end);
   disj_splitL=(fun lr -> (lr :> <start_smoking : _>));
   disj_splitR=(fun lr -> (lr :> <exit : _>));
  }

let playAsA =
  {obj={make_obj=(fun f -> object method playAsA=f end);
        call_obj=(fun o -> o#playAsA)};
   var={make_var=(fun v -> `playAsA(v)); match_var=(function `playAsA(v) -> Some v | _ -> None)}}

let playAsB =
  {obj={make_obj=(fun f -> object method playAsB=f end);
        call_obj=(fun o -> o#playAsB)};
   var={make_var=(fun v -> `playAsB(v)); match_var=(function `playAsB(v) -> Some v | _ -> None)}}

let playAsC =
  {obj={make_obj=(fun f -> object method playAsC=f end);
        call_obj=(fun o -> o#playAsC)};
   var={make_var=(fun v -> `playAsC(v)); match_var=(function `playAsC(v) -> Some v | _ -> None)}}

let playAsA_playAsB_or_playAsC =
  {disj_concat=(fun l r -> object method playAsA=l#playAsA method playAsB=l#playAsB method playAsC=r#playAsC end);
   disj_splitL=(fun lr -> (lr :> <playAsA : _; playAsB : _>));
   disj_splitR=(fun lr -> (lr :> <playAsC : _>));
  }

let playAsA_or_playAsB =
  {disj_concat=(fun l r -> object method playAsA=l#playAsA method playAsB=r#playAsB end);
   disj_splitL=(fun lr -> (lr :> <playAsA : _;>));
   disj_splitR=(fun lr -> (lr :> <playAsB : _>));
  }

let map =
  {obj={make_obj=(fun f -> object method map=f end);
        call_obj=(fun o -> o#map)};
   var={make_var=(fun v -> `map(v)); match_var=(function `map(v) -> Some v | _ -> None)}}

let work =
  {obj={make_obj=(fun f -> object method work=f end);
        call_obj=(fun o -> o#work)};
   var={make_var=(fun v -> `work(v)); match_var=(function `work(v) -> Some v | _ -> None)}}

let done_ =
  {obj={make_obj=(fun f -> object method done_=f end);
        call_obj=(fun o -> o#done_)};
   var={make_var=(fun v -> `done_(v)); match_var=(function `done_(v) -> Some v | _ -> None)}}

let work_or_stop =
  {disj_concat=(fun l r -> object method work=l#work method stop=r#stop end);
   disj_splitL=(fun lr -> (lr :> <work : _;>));
   disj_splitR=(fun lr -> (lr :> <stop : _>));
  }

let result_or_done =
  {disj_concat=(fun l r -> object method result=l#result method done_=r#done_ end);
   disj_splitL=(fun lr -> (lr :> <result : _;>));
   disj_splitR=(fun lr -> (lr :> <done_ : _>));
  }

let knock =
  {obj={make_obj=(fun f -> object method knock=f end);
        call_obj=(fun o -> o#knock)};
   var={make_var=(fun v -> `knock(v)); match_var=(function `knock(v) -> Some v | _ -> None)}}

let deliver =
  {obj={make_obj=(fun f -> object method deliver=f end);
        call_obj=(fun o -> o#deliver)};
   var={make_var=(fun v -> `deliver(v)); match_var=(function `deliver(v) -> Some v | _ -> None)}}

let maketoy =
  {obj={make_obj=(fun f -> object method maketoy=f end);
        call_obj=(fun o -> o#maketoy)};
   var={make_var=(fun v -> `maketoy(v)); match_var=(function `maketoy(v) -> Some v | _ -> None)}}

let start =
  {obj={make_obj=(fun f -> object method start=f end);
        call_obj=(fun o -> o#start)};
   var={make_var=(fun v -> `start(v)); match_var=(function `start(v) -> Some v | _ -> None)}}

let password =
  {obj={make_obj=(fun f -> object method password=f end);
        call_obj=(fun o -> o#password)};
   var={make_var=(fun v -> `password(v)); match_var=(function `password(v) -> Some v | _ -> None)}}

let again =
  {obj={make_obj=(fun f -> object method again=f end);
        call_obj=(fun o -> o#again)};
   var={make_var=(fun v -> `again(v)); match_var=(function `again(v) -> Some v | _ -> None)}}

let cancel =
  {obj={make_obj=(fun f -> object method cancel=f end);
        call_obj=(fun o -> o#cancel)};
   var={make_var=(fun v -> `cancel(v)); match_var=(function `cancel(v) -> Some v | _ -> None)}}

let login_or_cancel =
  {disj_concat=(fun l r -> object method login=l#login method cancel=r#cancel end);
   disj_splitL=(fun lr -> (lr :> <login : _;>));
   disj_splitR=(fun lr -> (lr :> <cancel : _>));
  }

let auth_or_again =
  {disj_concat=(fun l r -> object method auth=l#auth method again=r#again end);
   disj_splitL=(fun lr -> (lr :> <auth : _;>));
   disj_splitR=(fun lr -> (lr :> <again : _>));
  }

let query_or_dummy =
  {disj_concat=(fun l r -> object method query=l#query method dummy=r#dummy end);
   disj_splitL=(fun lr -> (lr :> <query : _;>));
   disj_splitR=(fun lr -> (lr :> <dummy : _>));
  }

let answer =
  {obj={make_obj=(fun f -> object method answer=f end);
        call_obj=(fun o -> o#answer)};
   var={make_var=(fun v -> `answer(v)); match_var=(function `answer(v) -> Some v | _ -> None)}}
