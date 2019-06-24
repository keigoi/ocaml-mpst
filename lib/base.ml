
type ('lr, 'l, 'r) obj_merge =
  {obj_merge: 'l -> 'r -> 'lr;
   obj_splitL: 'lr -> 'l;
   obj_splitR: 'lr -> 'r;
  }

type ('la,'va) method_ =
  {make_obj: 'va -> 'la;
   call_obj: 'la -> 'va}

type close = Close

type 'a one = One__

type tag = {tag:Obj.t}
let make_tag : 'v. ('v -> [>]) -> tag = fun f ->
  {tag=Obj.repr (f (Obj.magic ()))}
