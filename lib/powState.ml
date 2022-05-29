open Rows
open State

let determinise_list (type a) ctx (id : a State.id) (hds : a State.t lazy_t list)
    =
  match Context.lookup ctx id with
  | Some v -> v
  | None ->
      let hd =
        lazy
          (let { st_ops = (module D : StateOp with type a = a); _ } =
             Lazy.force (List.hd hds)
           in
           let hds = List.map (fun hd -> (Lazy.force hd).st) hds in
           let st =
             match hds with
             | [ hd ] -> D.determinise ctx hd
             | hd :: hds ->
                 D.determinise ctx @@ List.fold_left (D.merge ctx) hd hds
             | [] -> failwith "impossible: empty_merge"
           in
           { st; st_ops = (module D : StateOp with type a = a) })
      in
      Context.add_binding ctx id hd;
      hd

type _ t =
  | Deterministic : 'obj State.id * 'obj State.t lazy_t -> 'obj t
      (** A State.t with deterministic transitions. Note that the following states
          are not necessarily deterministic. *)
  | Epsilon : 'a t list -> 'a t  (** Epsilon transitions (i.e. merging) *)
  | InternalChoice : 'lr State.id * ('lr, 'l, 'r) disj * 'l t * 'r t -> 'lr t
      (** Internal choice, splitted by a disjoint concatenation (disj) *)
  | Lazy : 'a t lazy_t -> 'a t  (** Start of the fix-loop *)

exception UnguardedLoop

let unit =
  Deterministic
    ( Context.new_key (),
      Lazy.from_val
        { st = (); st_ops = (module Unit : StateOp with type a = unit) } )

let make_deterministic id obj = Deterministic (id, obj)
let merge l r = Epsilon [ l; r ]

let make_internal_choice disj l r =
  InternalChoice (Context.new_key (), disj, l, r)

let make_lazy t = Lazy t

type 'a merged_or_backward_epsilon =
  ('a State.id * 'a State.t lazy_t list, 'a t list) Either.t

let rec mem_phys k = function x :: xs -> k == x || mem_phys k xs | [] -> false
let fail_unguarded () = raise UnguardedLoop
let ret_merged x = Either.Left x
let ret_backward_epsilon x = Either.Right x

let filter_cycles ~visited backward =
  let backward =
    (* filter out backward epsilon transitions pointing to known states *)
    List.filter (fun id -> mem_phys id visited) backward
  in
  if List.length backward > 0 then
    (* there're some backward links yet -- return them *)
    ret_backward_epsilon backward
  else (* no backward epsilons anymore: unguarded recursion! *)
    fail_unguarded ()

let rec epsilon_closure :
    type a.
    context:context -> visited:a t list -> a t -> a merged_or_backward_epsilon =
 fun ~context ~visited st ->
  if mem_phys st visited then
    (* epsilon-transition to the visited state -- return it as a 'backward' link *)
    ret_backward_epsilon [ st ]
  else
    match st with
    | Deterministic (sid, h) -> ret_merged (sid, [ h ])
    | InternalChoice (sid, disj, tl, tr) ->
        ret_merged
          (sid, [ lazy (determinise_internal_choice_core context disj tl tr) ])
    | Lazy t -> (
        (* beginning of the fix-loop. *)
        match
          epsilon_closure ~context ~visited:(st :: visited) (Lazy.force t)
        with
        | Left _ as t -> t
        | Right backward -> filter_cycles ~visited backward)
    | Epsilon sts ->
        (* epsilon transitions: compute epsilon-closure *)
        let hds, backward =
          List.partition_map
            (epsilon_closure ~context ~visited:(st :: visited))
            sts
        in
        if List.length hds > 0 then
          (* concrete transitons found - return the merged state, discarding the backward links ==== *)
          let ids, hds = List.split hds in
          let id =
            List.fold_left Context.union_keys (List.hd ids) (List.tl ids)
          in
          ret_merged (id, List.concat hds)
        else
          (* all transitions are epsilon - verify guardedness ==== *)
          let backward = List.concat backward in
          filter_cycles ~visited backward

and determinise_internal_choice_core :
    type lr l r. context -> (lr, l, r) disj -> l t -> r t -> lr State.t =
 fun context disj tl tr ->
  let ( _idl,
        (lazy { st = (hl : l); st_ops = (module DL : StateOp with type a = l) })
      ) =
    determinise_core_ context tl
  and ( _idr,
        (lazy { st = (hr : r); st_ops = (module DR : StateOp with type a = r) })
      ) =
    determinise_core_ context tr
  in
  let module DLR = struct
    type a = lr

    let merge ctx lr1 lr2 =
      (* merge splitted ones *)
      let l = DL.merge ctx (disj.disj_splitL lr1) (disj.disj_splitL lr2) in
      let r = DR.merge ctx (disj.disj_splitR lr1) (disj.disj_splitR lr2) in
      (* then type-concatenate it *)
      disj.disj_concat l r

    let determinise ctx lr =
      let l = DL.determinise ctx (disj.disj_splitL lr) in
      let r = DR.determinise ctx (disj.disj_splitR lr) in
      disj.disj_concat l r

    let force ctx lr =
      DL.force ctx (disj.disj_splitL lr);
      DR.force ctx (disj.disj_splitR lr)

    let to_string ctx lr =
      DL.to_string ctx (disj.disj_splitL lr)
      ^ " (+) "
      ^ DR.to_string ctx (disj.disj_splitR lr)
  end in
  let lr = disj.disj_concat hl hr in
  { st = lr; st_ops = (module DLR : StateOp with type a = lr) }

and determinise_core_ : type s. context -> s t -> s State.id * s State.t lazy_t =
 fun context st ->
  match epsilon_closure ~context ~visited:[] st with
  | Left (sid, hds) -> (sid, determinise_list context sid hds)
  | Right _ -> fail_unguarded ()

let determinise (type a) (t : a t) =
  let _sid, h = determinise_core_ (Context.make ()) t in
  let { st; st_ops = (module M : StateOp with type a = a) } = Lazy.force h in
  M.force (Context.make ()) st;
  st

let determinise_core ctx t =
  let id, st = determinise_core_ ctx t in
  make_deterministic id st

let merge_det (type a) ctx (id : a State.id) (l : a State.t lazy_t)
    (r : a State.t lazy_t) =
  determinise_list ctx id [ l; r ]

let merge_core ctx l r =
  let idl, dl = determinise_core_ ctx l and idr, dr = determinise_core_ ctx r in
  let id = Context.union_keys idl idr in
  make_deterministic id (merge_det ctx id dl dr)

let force_core (type a) ctx (t : a t) =
  match t with
  | Deterministic (sid, h) when Context.lookup ctx sid = None ->
      Context.add_binding ctx sid h;
      let { st; st_ops = (module M : StateOp with type a = a) } =
        Lazy.force h
      in
      M.force ctx st
  | Deterministic (_, _) -> ()
  | _ -> failwith "Impossible: force: channel not determinised"

let rec to_string_core : type a. context -> a t -> string =
 fun ctx -> function
  | Deterministic (sid, hd) ->
      if Lazy.is_val hd then (
        Context.add_binding ctx sid hd;
        let { st; st_ops = (module D : StateOp with type a = a) } =
          Lazy.force hd
        in
        D.to_string ctx st)
      else "<lazy_det>"
  | Epsilon ts -> List.map (to_string_core ctx) ts |> String.concat " [#] "
  | InternalChoice (_, _, l, r) ->
      let lstr = to_string_core ctx l and rstr = to_string_core ctx r in
      lstr ^ " (+#) " ^ rstr
  | Lazy t ->
      if Lazy.is_val t then to_string_core ctx (Lazy.force t) else "<lazy_loop>"

let ensure_determinised = function
  | Deterministic (_, t) -> (Lazy.force t).st
  | _ -> failwith "not determinised"

let to_string t = to_string_core (Context.make ()) t
