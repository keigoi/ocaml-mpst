include Rows

(** {b Message labels} for global combinators, which is a pair of a first-class
    method and a {i variant constructor}. A variant constructor is of form
    [(fun x -> `lab(x))], indicating how a variant value is constructed. *)
type ('obj, 'ot, 'var, 'vt) label = {
  obj : ('obj, 'ot) Rows.method_;
  var : ('var, 'vt) Rows.constr;
}
(** A message label [lab] is constructed by the following:

    {[
      let lab =
        {
          obj =
            {
              make_obj =
                (fun x ->
                  object
                    method lab = x
                  end);
              call_obj = (fun obj -> obj#lab);
            };
          var = (fun x -> `lab x);
        }
    ]}

    The same method and variant tag {i names} must be defined in a message
    label.

    Note that the variant part of the above has {i open (>)} type as following:

    {[ val lab : (< lab : 'mt >, 'mt, [> `lab of 'vt ], 'vt) label ]} *)
(* constraint 'var = [>] *)

(** The {b role type} for global combinators. *)
type ('t, 'u, 'ts, 'us, 'robj, 'mt) role = {
  role_index : ('t, 'u, 'ts, 'us) Hlist.idx;  (** The index of a role. *)
  role_label : ('robj, 'mt) Rows.method_;  (** The label of a role. *)
}
(** The role value is constructed from a {i role index} (represented by
    polymorphic lens) and a {i role label} (represented by a method). For
    example, a role value [r] of a role [R] with index [1] is defined as
    follows:

    {[
      let r =
        {
          role_index = Succ Zero;
          role_label =
            {
              make_obj =
                (fun x ->
                  object
                    method role_R = x
                  end);
              call_obj = (fun obj -> obj#role_R);
            };
        }
    ]}

    The first four type parameters are a type-level index {!idx} as a
    polymorphic lens. The last two are type of a first-class method {!method_}.

    The programmer must consistently assign indices to roles. In the other
    words, every index in the role values in a global protocol must have
    one-to-one correspondence to each role label. *)
