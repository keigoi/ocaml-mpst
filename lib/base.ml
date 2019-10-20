
type ('lr, 'l, 'r) disj_merge =
  {disj_merge: 'l -> 'r -> 'lr;
   disj_splitL: 'lr -> 'l;
   disj_splitR: 'lr -> 'r;
  }

type ('la,'va) method_ =
  {make_obj: 'va -> 'la;
   call_obj: 'la -> 'va}

type ('la,'lb,'va,'vb) label =
  {obj: ('la, 'va) method_;
   var: 'vb -> 'lb}

type 'k role_metainfo =
  {rm_index:int;
   rm_kind:'k;
   rm_size:int}

type (_,_,_,_) lens =
  Zero : ('a, 'b, [`cons of 'a * 'tl], [`cons of 'b * 'tl]) lens
| Succ : ('a, 'b, 'aa, 'bb) lens -> ('a, 'b, [`cons of 'hd * 'aa], [`cons of 'hd * 'bb]) lens

type ('robj,'c,'a,'b,'xs,'ys) role =
  {role_label: ('robj,'c) method_;
   role_index: ('a,'b,'xs,'ys) lens}

type 'a one = One__

type tag = {tag:Obj.t}
let make_tag : 'v. ('v -> [>]) -> tag = fun f ->
  {tag=Obj.repr (f (Obj.magic ()))}
