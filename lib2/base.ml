(** First-class methods *)
type ('obj,'mt) method_ =
  {make_obj: 'mt -> 'obj; call_obj: 'obj -> 'mt} (* constraint 'obj = < .. > *)

let compose_method m1 m2 =
  {make_obj=(fun v -> m1.make_obj (m2.make_obj v));
  call_obj=(fun obj -> m2.call_obj @@ m1.call_obj obj)}

(** Polymorphic lens; representing type-level indices *)
type (_,_,_,_) idx =
  Zero : ('a, 'b, [`cons of 'a * 'tl], [`cons of 'b * 'tl]) idx
| Succ : ('a, 'b, 'aa, 'bb) idx -> ('a, 'b, [`cons of 'hd * 'aa], [`cons of 'hd * 'bb]) idx

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

let rec find_physeq : 'a. 'a list -> 'a -> bool = fun xs y ->
  match xs with
  | x::xs -> if x==y then true else find_physeq xs y
  | [] -> false

let rec int_of_idx : type a b c d. (a,b,c,d) idx -> int = function
  | Zero -> 0
  | Succ l -> int_of_idx l + 1

let map_option f = function
  | Some x -> Some (f x)
  | None -> None

let of_option ~dflt = function
  | Some x -> x
  | None -> dflt

let option ~dflt ~f = function
  | Some x -> f x
  | None -> dflt
