(** First-class methods *)
type ('obj,'mt) method_ =
  {make_obj: 'mt -> 'obj; call_obj: 'obj -> 'mt} (* constraint 'obj = < .. > *)

let compose_method m1 m2 =
  {make_obj=(fun v -> m1.make_obj (m2.make_obj v));
  call_obj=(fun obj -> m2.call_obj @@ m1.call_obj obj)}

(** Type-level indices represented by polymorphic lenses *)
type ('ts, 't, 'us, 'u) idx =
  {get: 'ts -> 't; put: 'ts -> 'u -> 'us}

(** Message labels for global combinators. See examples. *)
type ('obj,'ot,'var,'vt) label =
  {obj: ('obj, 'ot) method_; var: 'vt -> 'var} (* constraint 'var = [>] *)

(** Role types for global combinators. See examples *)
type ('ts, 't, 'us, 'u, 'robj, 'mt) role =
  {
    role_index: ('ts,'t,'us,'u) idx;
    (** The index of a role. Zero is "('x1*'y*'z, 'x1, 'x2*'y*'z, 'x2) idx" For three-role case. *)
    role_label:('robj,'mt) method_
    (** The label of a role. Normally it looks like (<role_A: 't>, 't) method_  *)
  }

(** Disjoint concatenation/splitting of two objects  *)
type ('lr, 'l, 'r) disj =
  {disj_concat: 'l -> 'r -> 'lr;
   disj_splitL: 'lr -> 'l;
   disj_splitR: 'lr -> 'r;
  }
  (* constraint 'lr = < .. >
   * constraint 'l = < .. >
   * constraint 'r = < .. > *)
