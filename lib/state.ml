open Rows

module type Det0 = sig
  type a
  type context

  val determinise : context -> a list -> a
  val force : context -> a -> unit
  val to_string : context -> a -> string
end

module rec Context : sig
  type t

  module type Det1 = Det0 with type context := t

  type 'a det_state = {
    det_state : 'a;
    det_ops : (module Det1 with type a = 'a);
  }

  type 'a value = 'a det_state lazy_t

  include ContextF.S with type t := t and type 'a value := 'a Context.value
end = struct
  module Ctx = ContextF.Make (Context)
  include Ctx

  module type Det1 = Det0 with type context := t

  type 'a det_state = {
    det_state : 'a;
    det_ops : (module Det1 with type a = 'a);
  }

  type 'a value = 'a det_state lazy_t
end

module type DetState = Context.Det1

type context = Context.t
type 't state_id = 't Context.key

type 'a det_state = 'a Context.det_state = {
  det_state : 'a;
  det_ops : (module DetState with type a = 'a);
}

let merge_det_list (type a) ctx (id : a state_id)
    (hds : a det_state lazy_t list) =
  match Context.lookup ctx id with
  | Some v -> v
  | None ->
      let hd =
        lazy
          (let { det_ops = (module D : DetState with type a = a); _ } =
             Lazy.force (List.hd hds)
           in
           let det_state =
             D.determinise ctx
               (List.map (fun hd -> (Lazy.force hd).det_state) hds)
           in
           { det_state; det_ops = (module D : DetState with type a = a) })
      in
      Context.add_binding ctx id hd;
      hd

let merge_det (type a) ctx (id : a state_id) (l : a det_state lazy_t)
    (r : a det_state lazy_t) =
  merge_det_list ctx id [ l; r ]

type _ t =
  | Deterministic : 'obj state_id * 'obj det_state lazy_t -> 'obj t
      (** A state with deterministic transitions. Note that the following states
          are not necessarily deterministic. *)
  | Epsilon : 'a t list -> 'a t  (** Epsilon transitions (i.e. merging) *)
  | InternalChoice : 'lr state_id * ('lr, 'l, 'r) disj * 'l t * 'r t -> 'lr t
      (** Internal choice, splitted by a disjoint concatenation (disj) *)
  | Loop : 'a t lazy_t -> 'a t  (** Start of the fix-loop *)

exception UnguardedLoop

type 'a merged_or_backward_epsilon =
  ('a state_id * 'a det_state lazy_t list, 'a t list) Either.t

let rec mem_phys k = function x :: xs -> k == x || mem_phys k xs | [] -> false
let fail_unguarded () = raise UnguardedLoop

let rec epsilon_closure :
    type a.
    ctx:context -> visited:a t list -> a t -> a merged_or_backward_epsilon =
  let ret_merged x = Either.Left x
  and ret_backward_epsilon x = Either.Right x in
  let detect_cycles ~visited backward =
    let backward =
      (* filter out backward epsilon transitions pointing to known states *)
      List.filter (fun id -> mem_phys id visited) backward
    in
    if List.length backward > 0 then
      (* there're backward links yet -- return them *)
      ret_backward_epsilon backward
    else
      (* no backward epsilons anymore: unguarded recursion! *)
      fail_unguarded ()
  in
  fun ~ctx ~visited st ->
    if mem_phys st visited then
      (* epsilon-transition to the visited state -- return it as a 'backward' link *)
      ret_backward_epsilon [ st ]
    else
      match st with
      | Deterministic (sid, h) -> ret_merged (sid, [ h ])
      | InternalChoice (sid, disj, tl, tr) ->
          ret_merged
            (sid, [ lazy (determinise_internal_choice_core ctx disj tl tr) ])
      | Loop t -> (
          (* beginning of the fix-loop. *)
          match
            epsilon_closure ~ctx ~visited:(st :: visited) (Lazy.force t)
          with
          | Left (sid, hd) -> Left (sid, hd)
          | Right backward -> detect_cycles ~visited backward)
      | Epsilon sts ->
          (* epsilon transitions: compute epsilon-closure *)
          let hds, backward =
            List.partition_map
              (epsilon_closure ~ctx ~visited:(st :: visited))
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
            detect_cycles ~visited backward

and determinise_internal_choice_core :
    type lr l r. context -> (lr, l, r) disj -> l t -> r t -> lr det_state =
 fun ctx disj tl tr ->
  let ( _idl,
        (lazy
          {
            det_state = (hl : l);
            det_ops = (module DL : DetState with type a = l);
          }) ) =
    determinise_core ctx tl
  and ( _idr,
        (lazy
          {
            det_state = (hr : r);
            det_ops = (module DR : DetState with type a = r);
          }) ) =
    determinise_core ctx tr
  in
  let bin_merge ctx lr1 lr2 =
    (* merge splitted ones *)
    let l = DL.determinise ctx [ disj.disj_splitL lr1; disj.disj_splitL lr2 ] in
    let r = DR.determinise ctx [ disj.disj_splitR lr1; disj.disj_splitR lr2 ] in
    (* then type-concatenate it *)
    disj.disj_concat l r
  in
  let module DLR = struct
    type a = lr

    let determinise ctx lrs =
      List.fold_left (bin_merge ctx) (List.hd lrs) (List.tl lrs)

    let force ctx lr =
      DL.force ctx (disj.disj_splitL lr);
      DR.force ctx (disj.disj_splitR lr)

    let to_string ctx lr =
      DL.to_string ctx (disj.disj_splitL lr)
      ^ " (+) "
      ^ DR.to_string ctx (disj.disj_splitR lr)
  end in
  let lr = disj.disj_concat hl hr in
  { det_state = lr; det_ops = (module DLR : DetState with type a = lr) }

and determinise_core : type s. context -> s t -> s state_id * s det_state lazy_t
    =
 fun ctx st ->
  match epsilon_closure ~ctx ~visited:[] st with
  | Left (sid, hds) -> (sid, merge_det_list ctx sid hds)
  | Right _ -> fail_unguarded ()

let determinise (type a) (t : a t) =
  let _, h = determinise_core (Context.make ()) t in
  let { det_state; det_ops = (module M : DetState with type a = a) } =
    Lazy.force h
  in
  M.force (Context.make ()) det_state;
  det_state

let ensure_determinised = function
  | Deterministic (_, t) -> (Lazy.force t).det_state
  | _ -> failwith "not determinised"

let force_core (type a) ctx (t : a t) =
  match t with
  | Deterministic (sid, h) when Context.lookup ctx sid = None ->
      Context.add_binding ctx sid h;
      let { det_state; det_ops = (module M : DetState with type a = a) } =
        Lazy.force h
      in
      M.force ctx det_state
  | Deterministic (_, _) -> ()
  | _ -> failwith "Impossible: force: channel not determinised"

let unit =
  let module D = struct
    type a = unit

    let determinise _ _ = ()
    let force _ _ = ()
    let to_string _ _ = "end"
  end in
  Deterministic
    ( Context.new_key (),
      Lazy.from_val
        { det_state = (); det_ops = (module D : DetState with type a = unit) }
    )

let make_deterministic id obj = Deterministic (id, obj)
let merge l r = Epsilon [ l; r ]

let make_internal_choice disj l r =
  InternalChoice (Context.new_key (), disj, l, r)

let make_loop t = Loop t

let rec to_string_core : type a. context -> a t -> string =
 fun ctx -> function
  | Deterministic (sid, hd) ->
      if Lazy.is_val hd then (
        Context.add_binding ctx sid hd;
        let { det_state; det_ops = (module D : DetState with type a = a) } =
          Lazy.force hd
        in
        D.to_string ctx det_state)
      else "<lazy_det>"
  | Epsilon ts -> List.map (to_string_core ctx) ts |> String.concat " [#] "
  | InternalChoice (_, _, l, r) ->
      let lstr = to_string_core ctx l and rstr = to_string_core ctx r in
      lstr ^ " (+#) " ^ rstr
  | Loop t ->
      if Lazy.is_val t then to_string_core ctx (Lazy.force t) else "<lazy_loop>"

let to_string t = to_string_core (Context.make ()) t

let try_merge_det (type a) ctx (id : a state_id) constrA constrB headA headB =
  let { det_state = headA; det_ops = (module D : DetState with type a = a) } =
    Lazy.force headA
  and { det_state = headB; _ } = Lazy.force headB in
  match Rows.cast_if_constrs_are_same constrA constrB headB with
  | Some contB_body -> begin
      match Context.lookup ctx id with
      | Some v -> Some v
      | None ->
          let det_state = D.determinise ctx [ headA; contB_body ] in
          let det =
            Lazy.from_val
              { det_state; det_ops = (module D : DetState with type a = a) }
          in
          Context.add_binding ctx id det;
          Some det
    end
  | None -> None
