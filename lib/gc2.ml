open Types

module PolyAssoc (V : sig
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
end

module NameHash_ = PolyAssoc (struct
  type 'a t = 'a State.t
end)

module NameUnify = Unify.Make (NameHash_)

(* module rec StateHash : sig
     type 'a key
   end = PolyAssoc (struct
     type 'a t = 'a State.nondet
   end)

   and State : sig
     type 'a nondet
   end *)
module StateHash = PolyAssoc (struct
  type 'a t = 'a
end)

(* module Do (X : sig
     type 'a t

     val make_obj : 'm t -> ('obj, 'm t) method_ -> 'obj t
     val make_val : 'v -> 'v t
   end) =
   struct
     open X

     let meth_m =
       {
         call_obj = (fun o -> o#m);
         make_obj =
           (fun v ->
             object
               method m = v
             end);
       }

     let o = make_obj (make_obj (make_val 1) meth_m) meth_m
   end

   module M2 = struct
     type 'a t = Obj : 'm t * ('obj, 'm t) method_ -> 'obj t | End : 'v -> 'v t

     let make_obj v mt = Obj (v, mt)
     let make_val v = End v
   end

   module M3 = struct
     module M4 = Do (M2)
     include M4

     let x = M4.o
   end *)
type 'a keyset = 'a StateHash.key * 'a StateHash.key list

module type STATE = sig
  type _ t
  type 'var inp
  type 's out

  val inp_trans :
    'obj keyset ->
    ('obj, 'var inp) method_ ->
    ('var, 'k) constr ->
    unit NameUnify.t ->
    'k t ->
    'obj t

  val out_trans :
    'obj keyset ->
    ('obj, 'mt) method_ ->
    ('mt, 'k out) method_ ->
    unit NameUnify.t ->
    'k t ->
    'obj t

  val internal_choice :
    'lr keyset -> ('lr, 'l, 'r) disj -> 'l t -> 'r t -> 'lr t

  val merge : 's t -> 's t -> 's t
  val unbound : unit -> 's t
  val make_lazy : 's t lazy_t -> 's t
  val unit : unit t
end

module Global (State : STATE) = struct
  open State

  type all_unit = [ `cons of unit * 'a ] as 'a

  module Open = struct
    type _ t =
      | [] : all_unit t
      | ( :: ) : (unit, 'b, 'bb, 'cc) Hlist.idx * 'bb t -> 'cc t
  end

  type 't global =
    | Comm :
        ('a, 'b, 'c, 'd, 'e, 'f inp) role
        * ('g, 'e, 'h, 'c, 'b, 'i) role
        * ('i, 'a out, 'f, 'g) label
        * 'h global
        -> 'd global
    | ChoiceAt :
        ('a, 'b, 'c, 'd, 'e, 'f) role
        * ('b, 'g, 'h) disj
        * (('g, unit, 'i, 'c, 'j, 'k) role * 'i global)
        * (('h, unit, 'l, 'c, 'm, 'n) role * 'l global)
        -> 'd global
    | PartialFinish : 'b Open.t * 'b global -> 'b global

  module Sessions = Hlist.Make (State)
  include Sessions

  let rec merge_seq : type t. t seq -> t seq -> t seq =
   fun ls rs ->
    match (ls, rs) with
    | Sessions.[], _ -> []
    | _, [] -> []
    | l :: ls, r :: rs -> merge l r :: merge_seq ls rs

  let rec eval_ : type t. t global -> t seq =
   fun g ->
    match g with
    | Comm (ra, rb, lab, g) ->
        let key = NameUnify.newkey () in
        let g = eval_ g in
        let b = seq_get rb.role_index g in
        let g =
          seq_put rb.role_index g
            (inp_trans (StateHash.newkey (), []) ra.role_label lab.var key b)
        in
        let a = seq_get ra.role_index g in
        let g =
          seq_put ra.role_index g
            (out_trans (StateHash.newkey (), []) rb.role_label lab.obj key a)
        in
        g
    | ChoiceAt (ra, disj, (ra1, g1), (ra2, g2)) ->
        let g1 = eval_ g1 in
        let g2 = eval_ g2 in
        let a1 = seq_get ra1.role_index g1 and a2 = seq_get ra2.role_index g2 in
        let g1 = seq_put ra1.role_index g1 unit
        and g2 = seq_put ra2.role_index g2 unit in
        let g = merge_seq g1 g2 in
        let a = internal_choice (StateHash.newkey (), []) disj a1 a2 in
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
                State.make_lazy (lazy (seq_get2 idx (Lazy.force keeps)))
              and rest =
                (* rest of the states *)
                lazy (seq_put2 idx (Lazy.force keeps) State.unit)
              in
              let states = finish ~keep_idxs:rest_idxs ~keeps:rest in
              (* put the state and return it *)
              seq_put idx states state
        in
        finish ~keep_idxs ~keeps:(lazy (eval_ keeps))

  (* let rec session_merge : type x. x Sessions.seq -> x Sessions.seq -> x Sessions.seq =
     let open Sessions in
     fun l r ->
      match (l, r) with
      | _ :: _, _ ->
          let hd = State.merge (seq_head l) (seq_head r) in
          let tl = session_merge (seq_tail l) (seq_tail r) in
          hd :: tl
      | _, _ :: _ -> session_merge r l
      | [], [] -> []
  *)
end

module State = struct
  type _ t =
    | Epsilon : 'a t list -> 'a t
    | Deterministic : 'obj keyset * 'obj trans -> 'obj t
    | InternalChoice : 'lr keyset * ('lr, 'l, 'r) disj * 'l t * 'r t -> 'lr t

  and _ trans =
    | OutTrans :
        ('obj, 'mt) method_ * ('mt, 'k out) method_ * 'k out
        -> 'obj trans
    | InpTrans : ('obj, 'var inp) method_ * 'var inp -> 'obj trans
    | End : unit trans

  and 'var inp = 'var inp0 list
  and _ inp0 = Inp0 : ('var, 's) constr * 'v NameUnify.t * 's t -> 'var inp0
  and _ out = Out : unit NameUnify.t * 's t -> 's out

  let out_trans statekey role lab name s =
    Deterministic (statekey, OutTrans (role, lab, Out (name, s)))

  let inp_trans statekey role constr name s =
    Deterministic (statekey, InpTrans (role, [ Inp0 (constr, name, s) ]))

  let internal_choice statekey disj l r = InternalChoice (statekey, disj, l, r)
  let merge l r = Epsilon [ l; r ]
  let unit = Deterministic ((StateHash.newkey (), []), End)
end

include Global (State)

module State = struct
  type _ t = Deterministic : 'obj keyset * 'obj -> 'obj t
  (* | Epsilon : 'a t list -> 'a t
     | InternalChoice : 'lr keyset * ('lr, 'l, 'r) disj * 'l t * 'r t -> 'lr t *)

  (* and _ trans =
     | OutTrans :
         ('obj, 'mt) method_ * ('mt, ('v, 'k) out) method_ * ('v, 'k) out
         -> 'obj trans
     | InpTrans : ('obj, 'var inp) method_ * 'var inp -> 'obj trans
     | End : unit trans *)
  and 'var inp = 'var inp0 list
  and _ inp0 = Inp0 : ('var, 's) constr * 'v NameUnify.t * 's t -> 'var inp0
  and (_, _) out = Out : 'v NameUnify.t * 's t -> ('v, 's) out

  let out_trans statekey role lab name s =
    Deterministic (statekey, role.make_obj (lab.make_obj @@ Out (name, s)))

  let inp_trans statekey role constr name s =
    Deterministic (statekey, role.make_obj List.[ Inp0 (constr, name, s) ])

  let internal_choice statekey disj (Deterministic (_, l))
      (Deterministic (_, r)) =
    Deterministic (statekey, disj.disj_concat l r)

  (* let merge l r = Epsilon [ l; r ] *)
  let unit = Deterministic ((StateHash.newkey (), []), ())
end

module Util = struct
  let a =
    {
      role_label =
        {
          make_obj =
            (fun v ->
              object
                method role_A = v
              end);
          call_obj = (fun o -> o#role_A);
        };
      role_index = Zero;
    }

  let b =
    {
      role_label =
        {
          make_obj =
            (fun v ->
              object
                method role_B = v
              end);
          call_obj = (fun o -> o#role_B);
        };
      role_index = Succ Zero;
    }

  let c =
    {
      role_label =
        {
          make_obj =
            (fun v ->
              object
                method role_C = v
              end);
          call_obj = (fun o -> o#role_C);
        };
      role_index = Succ (Succ Zero);
    }

  let d =
    {
      role_label =
        {
          make_obj =
            (fun v ->
              object
                method role_D = v
              end);
          call_obj = (fun o -> o#role_D);
        };
      role_index = Succ (Succ (Succ Zero));
    }

  let msg =
    {
      obj =
        {
          make_obj =
            (fun f ->
              object
                method msg = f
              end);
          call_obj = (fun o -> o#msg);
        };
      var =
        {
          make_var = (fun v -> `msg v);
          match_var = (function `msg v -> Some v | _ -> None);
        };
    }

  let left =
    {
      obj =
        {
          make_obj =
            (fun f ->
              object
                method left = f
              end);
          call_obj = (fun o -> o#left);
        };
      var =
        {
          make_var = (fun v -> `left v);
          match_var = (function `left v -> Some v | _ -> None);
        };
    }

  let right =
    {
      obj =
        {
          make_obj =
            (fun f ->
              object
                method right = f
              end);
          call_obj = (fun o -> o#right);
        };
      var =
        {
          make_var = (fun v -> `right v);
          match_var = (function `right v -> Some v | _ -> None);
        };
    }

  let middle =
    {
      obj =
        {
          make_obj =
            (fun f ->
              object
                method middle = f
              end);
          call_obj = (fun o -> o#middle);
        };
      var =
        {
          make_var = (fun v -> `middle v);
          match_var = (function `middle v -> Some v | _ -> None);
        };
    }

  let ping =
    {
      obj =
        {
          make_obj =
            (fun f ->
              object
                method ping = f
              end);
          call_obj = (fun o -> o#ping);
        };
      var =
        {
          make_var = (fun v -> `ping v);
          match_var = (function `ping v -> Some v | _ -> None);
        };
    }

  let pong =
    {
      obj =
        {
          make_obj =
            (fun f ->
              object
                method pong = f
              end);
          call_obj = (fun o -> o#pong);
        };
      var =
        {
          make_var = (fun v -> `pong v);
          match_var = (function `pong v -> Some v | _ -> None);
        };
    }

  let fini =
    {
      obj =
        {
          make_obj =
            (fun f ->
              object
                method fini = f
              end);
          call_obj = (fun o -> o#fini);
        };
      var =
        {
          make_var = (fun v -> `fini v);
          match_var = (function `fini v -> Some v | _ -> None);
        };
    }

  let left_or_right =
    {
      disj_concat =
        (fun l r ->
          object
            method left = l#left
            method right = r#right
          end);
      disj_splitL = (fun lr -> (lr :> < left : _ >));
      disj_splitR = (fun lr -> (lr :> < right : _ >));
    }

  let right_or_left =
    {
      disj_concat =
        (fun l r ->
          object
            method right = l#right
            method left = r#left
          end);
      disj_splitL = (fun lr -> (lr :> < right : _ >));
      disj_splitR = (fun lr -> (lr :> < left : _ >));
    }

  let to_ m r1 r2 r3 =
    let ( ! ) x = x.role_label in
    {
      disj_concat =
        (fun l r ->
          !r1.make_obj (m.disj_concat (!r2.call_obj l) (!r3.call_obj r)));
      disj_splitL = (fun lr -> !r2.make_obj (m.disj_splitL @@ !r1.call_obj lr));
      disj_splitR = (fun lr -> !r3.make_obj (m.disj_splitR @@ !r1.call_obj lr));
    }

  let to_a m = to_ m a a a
  let to_b m = to_ m b b b
  let to_c m = to_ m c c c
  let to_d m = to_ m d d d

  let left_middle_or_right =
    {
      disj_concat =
        (fun l r ->
          object
            method left = l#left
            method middle = l#middle
            method right = r#right
          end);
      disj_splitL = (fun lr -> (lr :> < left : _ ; middle : _ >));
      disj_splitR = (fun lr -> (lr :> < right : _ >));
    }

  let left_or_middle =
    {
      disj_concat =
        (fun l r ->
          object
            method left = l#left
            method middle = r#middle
          end);
      disj_splitL = (fun lr -> (lr :> < left : _ >));
      disj_splitR = (fun lr -> (lr :> < middle : _ >));
    }

  let left_or_middle_right =
    {
      disj_concat =
        (fun l r ->
          object
            method left = l#left
            method middle = r#middle
            method right = r#right
          end);
      disj_splitL = (fun lr -> (lr :> < left : _ >));
      disj_splitR = (fun lr -> (lr :> < middle : _ ; right : _ >));
    }

  let middle_or_right =
    {
      disj_concat =
        (fun l r ->
          object
            method middle = l#middle
            method right = r#right
          end);
      disj_splitL = (fun lr -> (lr :> < middle : _ >));
      disj_splitR = (fun lr -> (lr :> < right : _ >));
    }
end

module Test (State : STATE) = struct
  open Global (State)
  open Util

  let g = Comm (a, b, left, Finish)
  let seq = eval_ g
end

(* let rec eval_ : type t. t global -> t seq =
   fun g ->
    match g with
    | Comm (ra, rb, lab, g) ->
        let key = NameUnify.newkey () in
        let g = eval_ g in
        let b = seq_get rb.role_index g in
        let g =
          seq_put rb.role_index g
            (Deterministic
               ( (StateHash.newkey (), []),
                 InpTrans (ra.role_label, [ Inp0 (lab.var, key, b) ]) ))
        in
        let a = seq_get ra.role_index g in
        let g =
          seq_put ra.role_index g
            (Deterministic
               ( (StateHash.newkey (), []),
                 OutTrans (rb.role_label, lab.obj, Out (key, a)) ))
        in
        g
    | ChoiceAt (ra, disj, (ra1, g1), (ra2, g2)) ->
        let g1 = eval_ g1 in
        let g2 = eval_ g2 in
        let a1 = seq_get ra1.role_index g1 and a2 = seq_get ra2.role_index g2 in
        let g = assert false in
        let a = InternalChoice ((StateHash.newkey (), []), disj, a1, a2) in
        seq_put ra.role_index g a
    | Finish -> [] *)

(* let rec merge_inp0 : type a. a inp0 -> a inp0 -> a inp0 option =
    fun l r ->
     match (l, r) with
     | Inp0 (constr1, name1, cont1), Inp0 (constr2, name2, cont2) -> (
         match Types.cast_if_constrs_are_same constr1 constr2 (name2, cont2) with
         | Some (name2, cont2) ->
             NameUnify.unify name1 name2;
             Some (Inp0 (constr1, name1, merge cont1 cont2))
         | None -> None)

   let rec merge_inp_inp0 : type a. a inp -> a inp0 -> a inp =
    fun inp inp0 ->
     match inp with
     | i0 :: inp -> (
         match merge_inp0 i0 inp0 with
         | Some i0 -> i0 :: inp
         | None -> i0 :: merge_inp_inp0 inp inp0)
     | [] -> [ inp0 ]

   let merge_trans : type a. a trans -> a trans -> a trans =
    fun l r ->
     match (l, r) with
     | InpTrans (role1, inps1), InpTrans (role2, inps2) ->
         InpTrans
           ( role1,
             List.fold_left merge_inp_inp0 inps1 (cast_obj role1 role2 inps2) )
     | OutTrans (role1, lab1, Out (name1, cont1)), OutTrans (role2, lab2, out2)
       ->
         let (Out (name2, cont2)) =
           cast_obj
             (compose_methods role1 lab1)
             (compose_methods role2 lab2)
             out2
         in
         NameUnify.unify name1 name2;
         OutTrans (role1, lab1, Out (name1, merge cont1 cont2))
     | End, End -> End
     | MixedTrans (disj, l1, r1), MixedTrans (disj2, l2, r2) ->
         (* let lr2 = disj2.disj_concat l2 r2 in *)
         assert false
     | _ -> assert false *)

(* type 'a merged_or_backward_epsilon =
     ('a keyset * 'a trans list, 'a t list) Either.t

   let rec mem_phys k = function
     | x :: xs -> k == x || mem_phys k xs
     | [] -> false

   let ret_merged x = Either.Left x
   and ret_backward_epsilon x = Either.Right x

   exception Unguarded of string

   let fail_unguarded str = raise (Unguarded str)

   let union_sorted_lists (xs : 'a list) (ys : 'a list) =
     let rec loop aux xs ys =
       match (xs, ys) with
       | x :: xs, y :: ys ->
           if x = y then loop (x :: aux) xs ys
           else if x < y then loop (x :: aux) xs (y :: ys)
           else loop (y :: aux) (x :: xs) ys
       | [], ys -> List.rev aux @ ys
       | xs, [] -> List.rev aux @ xs
     in
     loop [] xs ys

   let union_keys ((k1, ws1) : 'a keyset) ((k2, ws2) : 'a keyset) : 'a keyset =
     ((if k1 < k2 then k1 else k2), union_sorted_lists ws1 ws2)

   let rec epsilon_closure : type a. a t -> a keyset * a trans list =
     let rec loop : visited:a t list -> a t -> a merged_or_backward_epsilon =
      fun ~visited st ->
       if mem_phys st visited then ret_backward_epsilon [ st ]
       else
         match st with
         | Deterministic (sid, v) -> ret_merged (sid, [ v ])
         | Epsilon sts ->
             (* epsilon transitions: compute epsilon-closure *)
             let hds, cycles =
               List.partition_map (loop ~visited:(st :: visited)) sts
             in
             if List.length hds > 0 then
               (* concrete transitons found - return the merged state ==== *)
               let ids, hds = List.split hds in
               let id = List.fold_left union_keys (List.hd ids) (List.tl ids) in
               ret_merged (id, List.concat hds)
             else
               (* all transitions are epsilon - verify guardedness ==== *)
               let cycles = List.concat cycles in
               let cycles =
                 (* filter out epsilons pointing to myself *)
                 List.filter (fun id -> mem_phys id visited) cycles
               in
               if List.length cycles > 0 then
                 (* there're backward epsilons yet -- return it *)
                 ret_backward_epsilon cycles
               else
                 (* no backward epsilons anymore: unguarded recursion! *)
                 fail_unguarded "epsilon_closure: unguarded"
         | InternalChoice (sid, disj, tl, tr) ->
             let _idl, hls = epsilon_closure tl
             and _idr, hrs = epsilon_closure tr in
             let hl = merge_dets hls and hr = merge_dets hrs in
             let tlr = Mixed (disj, hl, hr) in
             ret_merged (sid, [ tlr ])
     in
     fun st ->
       match loop ~visited:[] st with
       | Left (sid, hds) -> (sid, hds)
       | Right _ -> fail_unguarded "epsilon_closure: unguarded" *)

(* let rec merge : type a. PolyAssoc.binding list -> a t -> a t -> a t = fun binding l r ->
   match l *)
