
type ('lr, 'l, 'r) obj_merge =
  {obj_merge: 'l -> 'r -> 'lr;
   obj_splitL: 'lr -> 'l;
   obj_splitR: 'lr -> 'r;
  }

type ('la,'va) method_ =
  {make_obj: 'va -> 'la;
   call_obj: 'la -> 'va}

let fork_child f x =
  if Unix.fork () = 0 then begin
      f x;
      exit 0;
    end else ()

type close = Close

type 'a one = One__
