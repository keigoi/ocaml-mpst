open Types

module type STATE = sig
  type _ t
  type +'x val_
  type _ inp
  type (_, _) out

  val unit : unit t
end

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

module State = struct
  type 'x val_ = 'x NameUnify.t
  type 'a keyset = 'a StateHash.key * 'a StateHash.key list

  type _ t =
    | Epsilon : 'a t list -> 'a t
    | Deterministic : 'obj keyset * 'obj trans -> 'obj t
    | InternalChoice : 'lr keyset * ('lr, 'l, 'r) disj * 'l t * 'r t -> 'lr t

  and _ trans =
    | MixedTrans : ('lr, 'l, 'r) disj * 'l trans * 'r trans -> 'lr trans
    | OutTrans :
        ('obj, 'mt) method_ * ('mt, ('v, 'k) out) method_ * ('v, 'k) out
        -> 'obj trans
    | InpTrans : ('obj, 'var inp) method_ * 'var inp -> 'obj trans
    | End : unit trans

  and 'var inp = 'var inp0 list

  and _ inp0 =
    | Inp0 : ('var, 'v val_ * 's t) constr * 'v NameUnify.t * 's t -> 'var inp0

  and (_, _) out = Out : 'v NameUnify.t * 's t -> ('v, 's) out

  let rec merge_inp0 : type a. a inp0 -> a inp0 -> a inp0 option =
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
    | _ -> assert false

  let unit = Deterministic ((StateHash.newkey (), []), EndState)

  type 'a merged_or_backward_epsilon =
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
      | Right _ -> fail_unguarded "epsilon_closure: unguarded"

  (* let rec merge : type a. PolyAssoc.binding list -> a t -> a t -> a t = fun binding l r ->
     match l *)
end

include State

type 't global =
  | Comm :
      ('a, 'b, 'c, 'd, 'e, 'f inp) role
      * ('g, 'e, 'h, 'c, 'b, 'i) role
      * ('i, ('v, 'a) out, 'f, 'v val_ * 'g) label
      * 'h global
      -> 'd global
  | ChoiceAt :
      ('a, 'b, 'c, 'd, 'e, 'f) role
      * ('b, 'g, 'h) disj
      * (('g, unit, 'i, 'c, 'j, 'k) role * 'i global)
      * (('h, unit, 'l, 'c, 'm, 'n) role * 'l global)
      -> 'd global
  | Finish : ([ `cons of unit * 'a ] as 'a) global

type 'a sess = { sess : 'a; merge : 'a -> 'a -> 'a }

module Sessions = Hlist.Make (State)
open Sessions

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
let rec eval_ : type t. state_id:int -> t global -> int * t seq =
 fun ~state_id g ->
  match g with
  | Comm (ra, rb, lab, g) ->
      let current_state_id = state_id in
      let state_id = state_id + 1 in
      let key = NameUnify.newkey () in
      let state_id, g = eval_ ~state_id g in
      let b = seq_get rb.role_index g in
      let g =
        seq_put rb.role_index g
          (Deterministic
             (current_state_id, InpState (ra.role_label, Inp (key, b, lab.var))))
      in
      let a = seq_get ra.role_index g in
      let g =
        seq_put ra.role_index g
          (Deterministic
             (current_state_id, OutState (rb.role_label, lab.obj, Out (key, a))))
      in
      (state_id, g)
  | ChoiceAt (ra, disj, (ra1, g1), (ra2, g2)) ->
      let current_state_id = state_id in
      let state_id = state_id + 1 in
      let state_id, g1 = eval_ ~state_id g1 in
      let state_id, g2 = eval_ ~state_id g2 in
      let a1 = seq_get ra1.role_index g1 and a2 = seq_get ra2.role_index g2 in
      let g = assert false in
      let a = InternalChoice (current_state_id, disj, a1, a2) in
      (state_id, seq_put ra.role_index g a)
  | Finish -> (state_id, [])
