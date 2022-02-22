open Types

module type STATE = sig
  type _ t
  type 'var inp
  type 's out

  exception UnguardedLoop of string

  val inp :
    ('obj, 'var inp) method_ ->
    ('var, 'k) constr ->
    unit Name.t ->
    'k t ->
    'obj t

  val out :
    ('obj, 'mt) method_ ->
    ('mt, 'k out) method_ ->
    unit Name.t ->
    'k t ->
    'obj t

  val internal_choice : ('lr, 'l, 'r) disj -> 'l t -> 'r t -> 'lr t
  val merge : 's t -> 's t -> 's t
  val lazy_ : 's t lazy_t -> 's t
  val unit : unit t
  val determinise : 'a t -> 'a
  val select : 's out -> 's
  val branch : 'var inp -> 'var
end

type all_unit = [ `cons of unit * 'a ] as 'a

module Open = struct
  type _ t =
    | [] : all_unit t
    | ( :: ) : (unit, 'b, 'bb, 'cc, _, _) role * 'bb t -> 'cc t
end

module Make (State : STATE) = struct
  module Sessions = Hlist.Make (State)
  open Sessions

  type 's out = 's State.out
  type 's inp = 's State.inp

  exception UnguardedLoop = State.UnguardedLoop

  let select = State.select
  let branch = State.branch

  let close () = ()

  let ( --> ) ra rb lab g =
    let key = Name.make () in
    let b = seq_get rb.role_index g in
    let g = seq_put rb.role_index g (State.inp ra.role_label lab.var key b) in
    let a = seq_get ra.role_index g in
    let g = seq_put ra.role_index g (State.out rb.role_label lab.obj key a) in
    g

  let rec merge_seq : type t. t seq -> t seq -> t seq =
   fun ls rs ->
    match (ls, rs) with
    | Sessions.[], _ -> []
    | _, [] -> []
    | l :: ls, r :: rs -> State.merge l r :: merge_seq ls rs

  let choice_at ra disj (ra1, g1) (ra2, g2) =
    let a1 = seq_get ra1.role_index g1 and a2 = seq_get ra2.role_index g2 in
    let g1 = seq_put ra1.role_index g1 State.unit
    and g2 = seq_put ra2.role_index g2 State.unit in
    let g = merge_seq g1 g2 in
    let a = State.internal_choice disj a1 a2 in
    seq_put ra.role_index g a

  let partial_finish keep_idxs g =
    let rec make_partial_finish :
        type b. keep_idxs:b Open.t -> keeps:b seq lazy_t -> b seq =
     fun ~keep_idxs ~keeps ->
      match keep_idxs with
      | Open.[] -> Sessions.Hetero.[]
      | idx :: rest_idxs ->
          let state =
            (* get the state of the involved role *)
            State.lazy_ (lazy (seq_get2 idx.role_index (Lazy.force keeps)))
          and rest =
            (* rest of the states *)
            lazy (seq_put2 idx.role_index (Lazy.force keeps) State.unit)
          in
          let states = make_partial_finish ~keep_idxs:rest_idxs ~keeps:rest in
          (* put the state and return it *)
          seq_put idx.role_index states state
    in
    make_partial_finish ~keep_idxs ~keeps:g

  let fix_with keep_idxs f =
    let rec self = lazy (f (partial_finish keep_idxs self)) in
    Lazy.force self

  let finish = Sessions.Hetero.[]

  let rec extract : type u. u Sessions.seq -> u = function
    | Sessions.[] ->
        let rec nil = `cons ((), nil) in
        nil
    | st :: tail ->
        let hd = State.determinise st in
        let tl = extract tail in
        `cons (hd, tl)
end

module G = Make (State2)

module Global (State : STATE) = struct
  type 't global =
    | Comm :
        ('a, 'b, 'c, 'd, 'e, 'f State.inp) role
        * ('g, 'e, 'h, 'c, 'b, 'i) role
        * ('i, 'a State.out, 'f, 'g) label
        * 'h global
        -> 'd global
    | ChoiceAt :
        ('a, 'b, 'c, 'd, 'e, 'f) role
        * ('b, 'g, 'h) disj
        * (('g, unit, 'i, 'c, 'j, 'k) role * 'i global)
        * (('h, unit, 'l, 'c, 'm, 'n) role * 'l global)
        -> 'd global
    | PartialFinish : 'b Open.t * 'b global -> 'b global
    | Finish : all_unit global

  module Sessions = Hlist.Make (State)

  type 't seq = 't Sessions.seq

  let rec merge_seq : type t. t seq -> t seq -> t seq =
   fun ls rs ->
    match (ls, rs) with
    | Sessions.[], _ -> []
    | _, [] -> []
    | l :: ls, r :: rs -> State.merge l r :: merge_seq ls rs

  let rec eval : type t. t global -> t seq =
    let open Sessions in
    fun g ->
      match g with
      | Comm (ra, rb, lab, g) ->
          let key = Name.make () in
          let g = eval g in
          let b = seq_get rb.role_index g in
          let g =
            seq_put rb.role_index g (State.inp ra.role_label lab.var key b)
          in
          let a = seq_get ra.role_index g in
          let g =
            seq_put ra.role_index g (State.out rb.role_label lab.obj key a)
          in
          g
      | ChoiceAt (ra, disj, (ra1, g1), (ra2, g2)) ->
          let g1 = eval g1 in
          let g2 = eval g2 in
          let a1 = seq_get ra1.role_index g1
          and a2 = seq_get ra2.role_index g2 in
          let g1 = seq_put ra1.role_index g1 State.unit
          and g2 = seq_put ra2.role_index g2 State.unit in
          let g = merge_seq g1 g2 in
          let a = State.internal_choice disj a1 a2 in
          seq_put ra.role_index g a
      | PartialFinish (keep_idxs, keeps) ->
          let rec finish :
              type b. keep_idxs:b Open.t -> keeps:b seq lazy_t -> b seq =
           fun ~keep_idxs ~keeps ->
            match keep_idxs with
            | Open.[] -> Sessions.[]
            | idx :: rest_idxs ->
                let state =
                  (* get the state of the involved role *)
                  State.lazy_
                    (lazy (seq_get2 idx.role_index (Lazy.force keeps)))
                and rest =
                  (* rest of the states *)
                  lazy (seq_put2 idx.role_index (Lazy.force keeps) State.unit)
                in
                let states = finish ~keep_idxs:rest_idxs ~keeps:rest in
                (* put the state and return it *)
                seq_put idx.role_index states state
          in
          finish ~keep_idxs ~keeps:(lazy (eval keeps))
      | Finish -> []
end

(*
   module State0 = struct
     type _ t =
       | Epsilon : 'a t list -> 'a t
       | Deterministic : 'obj StateId.t * 'obj trans -> 'obj t
       | InternalChoice : 'lr StateId.t * ('lr, 'l, 'r) disj * 'l t * 'r t -> 'lr t
       | Lazy : 'a t lazy_t -> 'a t

     and _ trans =
       | OutTrans :
           ('obj, 'mt) method_ * ('mt, 'k out) method_ * 'k out
           -> 'obj trans
       | InpTrans : ('obj, 'var inp) method_ * 'var inp -> 'obj trans
       | End : unit trans

     and 'var inp = 'var inp0 list
     and _ inp0 = Inp0 : ('var, 's) constr * unit NameUnify.t * 's t -> 'var inp0
     and _ out = Out : unit NameUnify.t * 's t -> 's out

     let out_trans role lab name s =
       Deterministic (StateId.make (), OutTrans (role, lab, Out (name, s)))

     let inp_trans role constr name s =
       Deterministic (StateId.make (), InpTrans (role, [ Inp0 (constr, name, s) ]))

     let internal_choice disj l r = InternalChoice (StateId.make (), disj, l, r)
     let merge l r = Epsilon [ l; r ]
     let make_lazy t = Lazy t
     let unit = Deterministic (StateId.make (), End)
   end *)

(* module PolyAssoc (V : sig
     type 'a t
   end) : sig
     type 'a key
     type binding = B : 'a key * 'a V.t -> binding

     val newkey : unit -> 'a key
     val empty : binding list
     val lookup : binding list -> 'a key -> 'a V.t
   end = struct
     module Key = struct
       type _ t = ..
     end

     module type W = sig
       type t
       type _ Key.t += Key : t Key.t
     end

     type 'a key = (module W with type t = 'a)
     type binding = B : 'a key * 'a V.t -> binding
     type ('a, 'b) eq = Eq : ('a, 'a) eq

     let newkey () (type s) =
       let module M = struct
         type t = s
         type _ Key.t += Key : t Key.t
       end in
       (module M : W with type t = s)

     let eq (type r s) (r : r key) (s : s key) : (r, s) eq option =
       let module R = (val r : W with type t = r) in
       let module S = (val s : W with type t = s) in
       match R.Key with S.Key -> Some Eq | _ -> None

     let empty = []

     let lookup : type a. binding list -> a key -> a V.t =
      fun d k ->
       let rec find : binding list -> a V.t = function
         | [] -> raise Not_found
         | B (k', v) :: bs -> ( match eq k k' with Some Eq -> v | _ -> find bs)
       in
       find d
   end *)

(* module NameHash_ = PolyAssoc (struct
     type 'a t = 'a State.t
   end)

   module NameUnify = Unify.Make (NameHash_) *)
