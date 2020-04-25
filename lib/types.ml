(* see types.mli *)

type ('obj,'mt) method_ =
{make_obj: 'mt -> 'obj; call_obj: 'obj -> 'mt} (* constraint 'obj = < .. > *)

type ('a,'b,'xs,'ys) idx =
Zero : ('a, 'b, [`cons of 'a * 'tl], [`cons of 'b * 'tl]) idx
| Succ : ('a, 'b, 'aa, 'bb) idx -> ('a, 'b, [`cons of 'hd * 'aa], [`cons of 'hd * 'bb]) idx

type ('obj,'ot,'var,'vt) label =
{obj: ('obj, 'ot) method_; var: 'vt -> 'var} (* constraint 'var = [>] *)

type ('ts, 't, 'us, 'u, 'robj, 'mt) role =
{
    role_index: ('ts,'t,'us,'u) idx;
    role_label:('robj,'mt) method_
}

type ('lr, 'l, 'r) disj =
{disj_concat: 'l -> 'r -> 'lr;
disj_splitL: 'lr -> 'l;
disj_splitR: 'lr -> 'r;
}

type 'a one = One of 'a
