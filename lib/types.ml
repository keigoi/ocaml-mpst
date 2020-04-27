

(** 
    The basis for composing OCaml's structural types
    in a {i type-safe} manner.
    Skip this section if you don't define a custom role or label, 
    and not interested in the typing of global combinators.
  *)

(** Object's {b method} as a first-class value. *)
type ('obj,'mt) method_ =
{
    make_obj: 'mt -> 'obj;
    (** Constructor of an object ['obj] with a method of type ['mt]. *)
    call_obj: 'obj -> 'mt
    (** Destructor of an object ['obj], calling the method of type ['mt]. *)
} (* constraint 'obj = < .. > *)
(**
    Typically, it has the following polymorphic type:

    {[
        (<lab:'mt>, 'mt) method_
    ]}

    which denotes a method [lab]. It can be constructed as follows:

    {[
        {make_obj=(fun x -> object method lab=x end); call_obj=(fun obj -> obj#lab)}
    ]}
*)


(** {b Message labels} for global combinators, which is a pair of a first-class method and 
    a {i variant constructor}.
    A variant constructor is of form [(fun x -> `lab(x))], indicating how a variant value
    is constructed. 
  *)
type ('obj,'ot,'var,'vt) label =
{obj: ('obj, 'ot) method_; var: 'vt -> 'var} (* constraint 'var = [>] *)
(**
    A message label [lab] is constructed by the following:

    {[
        let lab = {
            obj={make_obj=(fun x -> object method lab=x end); call_obj=(fun obj -> obj#lab)};
            var=(fun x -> `lab(x))
        }
    ]}
    
    The same method and variant tag {i names} must be defined in a message label.

    Note that the variant part of the above has {i open (>)} type as following:

    {[
        val lab : (<lab: 'mt>, 'mt, [> `lab of 'vt], 'vt) label
    ]}
*)

(** The {b type-level indices} of a type-level [cons]-list, i.e., 
    it is just a type of (Peano) {i natural numbers} represented as
    [Zero], [(Succ Zero)], [(Succ (Succ Zero))] ... and so on. 
    The type denotes a {b polymorphic lens} defined using GADTs.
    A lens is a functional reference to a portion in a data structure.
    A polymorphic lens is capable of replacing a portion with a value of 
    {i different type}. 
  *)
type ('a,'b,'xs,'ys) idx =
| Zero : ('a, 'b, [`cons of 'a * 'tl], [`cons of 'b * 'tl]) idx
  (** Type-level zero. *)
| Succ : ('a, 'b, 'aa, 'bb) idx -> ('a, 'b, [`cons of 'hd * 'aa], [`cons of 'hd * 'bb]) idx
  (** Type-level succ. *)
(**
    The index type [('a,'b,'xs,'ys) idx] says that:
    - It refers to the portion ['a] in ['xs], and
    - If ['a] in ['xs] is replaced with ['b], ['ys] is obtained.

    The [Zero] constructor refers to the first element of a (type-level) cons-list.
    The list is expressed by a polymorphic variant [[`cons of 'hd * 'tl]],
    and [Zero] refers to the ['hd] part of a list.
    The type [Zero : ('a, 'b, [`cons of 'a * 'tl], [`cons of 'b * 'tl]) idx] says that
    the type of the head of list is ['a], and it could be replaced by ['b], obtaining
    the whole list type [[`cons of 'b * 'tl]].

    [Succ] is more complex. It takes a lens referring to some portion ['a] in ['aa],
    and makes another lens larger by one value, by nesting it inside [[`cons of 'hd * 'aa]].

    This type is originally inspired by Garrigue's {{: https://github.com/garrigue/safeio} Safeio} 
    and 
    {{: https://www.math.nagoya-u.ac.jp/~garrigue/papers/linocaml-201902.pdf} Linocaml (Fig. 6)}.
*)

(** The {b role type} for global combinators. *)
type ('t, 'u, 'ts, 'us, 'robj, 'mt) role =
{
    role_index: ('t, 'u, 'ts, 'us) idx;
    (** The index of a role. *)
    role_label:('robj,'mt) method_
    (** The label of a role. *)
}
(** The role value is constructed from a {i role index} (represented by polymorphic lens) 
    and a {i role label} (represented by a method).
    For example, a role value [r] of a role [R] with index [1] is defined as follows:

    {[
        let r = {
            role_index=Succ Zero;
            role_label={make_obj=(fun x -> object method role_R=x end);
                        call_obj=(fun obj -> obj#role_R)}
        }
    ]}

    The first four type parameters are a type-level index {! idx} as a polymorphic lens.
    The last two are type of a first-class method {! method_}.

    The programmer must consistently assign indices to roles. 
    In the other words, every index in the role values in a global protocol must have
    one-to-one correspondence to each role label.

*)

(** {b Disjoint concatenation} of two objects, used in the {! choice} combinator.  *)
type ('lr, 'l, 'r) disj =
{disj_concat: 'l -> 'r -> 'lr;
(** Concatenation of a object *)
disj_splitL: 'lr -> 'l;
(** Split the left hand part from the concatenated object. *)
disj_splitR: 'lr -> 'r;
(** Split the right hand part from the concatenated object. *)
}
(* constraint 'lr = < .. >
* constraint 'l = < .. >
* constraint 'r = < .. > *)
(**
    For example, the disjoint concatenation [left_or_right] of two object with 
    methods [left] and [right] respectively is defined by the following:

    {[
        let left_or_right = {
            disj_concat=(fun l r -> object method left=l#left method right=r#right end);
            disj_splitL=(fun obj -> obj#left);
            disj_splitR=(fun obj -> obj#right)
        }
    ]}

    The above has type [(<left:'t1; right:'t2>, <left:'t1>, <right:'t2>) disj].
*)


(**/**)

type 'a one = One of 'a
