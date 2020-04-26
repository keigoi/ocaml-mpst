
(** First-class methods. *)
type ('obj,'mt) method_ =
{make_obj: 'mt -> 'obj; call_obj: 'obj -> 'mt} (* constraint 'obj = < .. > *)

(** Polymorphic lens, representing type-level indices. *)
type ('a,'b,'xs,'ys) idx =
Zero : ('a, 'b, [`cons of 'a * 'tl], [`cons of 'b * 'tl]) idx
| Succ : ('a, 'b, 'aa, 'bb) idx -> ('a, 'b, [`cons of 'hd * 'aa], [`cons of 'hd * 'bb]) idx

(** Message labels for global combinators. *)
type ('obj,'ot,'var,'vt) label =
{obj: ('obj, 'ot) method_; var: 'vt -> 'var} (* constraint 'var = [>] *)

(** Role types for global combinators. *)
type ('ts, 't, 'us, 'u, 'robj, 'mt) role =
{
    role_index: ('ts,'t,'us,'u) idx;
    (** The index of a role. Zero is "('x1*'y*'z, 'x1, 'x2*'y*'z, 'x2) idx" For three-role case. *)
    role_label:('robj,'mt) method_
    (** The label of a role. Normally it looks like (<role_A: 't>, 't) method_  *)
}

(** Disjoint concatenation/splitting of two objects.  *)
type ('lr, 'l, 'r) disj =
{disj_concat: 'l -> 'r -> 'lr;
disj_splitL: 'lr -> 'l;
disj_splitR: 'lr -> 'r;
}
(* constraint 'lr = < .. >
* constraint 'l = < .. >
* constraint 'r = < .. > *)

(**/**)

type 'a one = One of 'a
