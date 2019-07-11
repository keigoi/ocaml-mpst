
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

type close = Close

type 'a one = One__

type tag = {tag:Obj.t}
let make_tag : 'v. ('v -> [>]) -> tag = fun f ->
  {tag=Obj.repr (f (Obj.magic ()))}
