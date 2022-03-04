open Rows

type 'a head = 'a Head.head = {
  head : 'a;
  determinise_list : Head.context -> 'a list -> 'a;
  force_traverse : Head.context -> 'a -> unit;
  to_string : Head.context -> 'a -> string;
}

type 't state_id = 't Head.key

type _ t =
  | Deterministic : 'obj state_id * 'obj head lazy_t -> 'obj t
      (** A state with deterministic transitions. Note that the following states
          are not necessarily deterministic. *)
  | Epsilon : 'a t list -> 'a t  (** Epsilon transitions (i.e. merging) *)
  | InternalChoice : 'lr state_id * ('lr, 'l, 'r) disj * 'l t * 'r t -> 'lr t
      (** Internal choice, splitted by a disjoint concatenation (disj) *)
  | Loop : 'a t lazy_t -> 'a t  (** Start of the fix-loop *)

exception UnguardedLoop

let deterministic id obj = Deterministic (id, obj)
let internal_choice disj l r = InternalChoice (Head.make_key (), disj, l, r)
let merge l r = Epsilon [ l; r ]
let loop t = Loop t

let force_traverse ctx t =
  match t with
  | Deterministic (sid, h) when Head.lookup ctx sid = None ->
      Head.add_binding ctx sid h;
      let h = Lazy.force h in
      h.force_traverse ctx h.head
  | Deterministic (_, _) -> ()
  | _ -> failwith "Impossible: force: channel not determinised"

let rec to_string : 'a. Head.context -> 'a t -> string =
 fun ctx -> function
  | Deterministic (sid, hd) ->
      if Lazy.is_val hd then (
        Head.add_binding ctx sid hd;
        let hd = Lazy.force hd in
        hd.to_string ctx hd.head)
      else "<lazy_det>"
  | Epsilon ts -> List.map (to_string ctx) ts |> String.concat " [#] "
  | InternalChoice (_, _, l, r) ->
      let lstr = to_string ctx l and rstr = to_string ctx r in
      lstr ^ " (+#) " ^ rstr
  | Loop t ->
      if Lazy.is_val t then to_string ctx (Lazy.force t) else "<lazy_loop>"

module Determinise : sig
  val determinise : Head.context -> 's t -> 's state_id * 's head lazy_t
end = struct
  type 'a merged_or_backward_epsilon =
    ('a state_id * 'a head lazy_t list, 'a t list) Either.t

  let rec mem_phys k = function
    | x :: xs -> k == x || mem_phys k xs
    | [] -> false

  let fail_unguarded () = raise UnguardedLoop

  let rec determinise : 's. Head.context -> 's t -> 's state_id * 's head lazy_t
      =
   fun ctx st ->
    match epsilon_closure ~ctx ~visited:[] st with
    | Left (sid, hds) -> (sid, Head.determinise_head_list ctx sid hds)
    | Right _ -> fail_unguarded ()

  and epsilon_closure :
      type a.
      ctx:Head.context ->
      visited:a t list ->
      a t ->
      a merged_or_backward_epsilon =
    let ret_merged x = Either.Left x
    and ret_backward_epsilon x = Either.Right x in
    let detect_cycles ~visited backward =
      let backward =
        (* filter out backward epsilon transitions pointing to known states *)
        List.filter (fun id -> mem_phys id visited) backward
      in
      if List.length backward > 0 then
        (* there're backward epsilons yet -- return it *)
        ret_backward_epsilon backward
      else
        (* no backward epsilons anymore: unguarded recursion! *)
        fail_unguarded ()
    in
    fun ~ctx ~visited st ->
      if mem_phys st visited then ret_backward_epsilon [ st ]
      else
        match st with
        | Deterministic (sid, h) -> ret_merged (sid, [ h ])
        | InternalChoice (sid, disj, tl, tr) ->
            ret_merged
              (sid, [ lazy (determinise_internal_choice ctx disj tl tr) ])
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
              (* concrete transitons found - return the merged state ==== *)
              let ids, hds = List.split hds in
              let id =
                List.fold_left Head.make_union_keys (List.hd ids) (List.tl ids)
              in
              ret_merged (id, List.concat hds)
            else
              (* all transitions are epsilon - verify guardedness ==== *)
              let backward = List.concat backward in
              detect_cycles ~visited backward

  and determinise_internal_choice :
        'lr 'l 'r.
        Head.context -> ('lr, 'l, 'r) disj -> 'l t -> 'r t -> 'lr head =
   fun ctx disj tl tr ->
    let _idl, hl = determinise ctx tl and _idr, hr = determinise ctx tr in
    let hl = Lazy.force hl and hr = Lazy.force hr in
    let bin_merge ctx lr1 lr2 =
      (* merge splitted ones *)
      let l =
        hl.determinise_list ctx [ disj.disj_splitL lr1; disj.disj_splitL lr2 ]
      in
      let r =
        hr.determinise_list ctx [ disj.disj_splitR lr1; disj.disj_splitR lr2 ]
      in
      (* then type-concatenate it *)
      disj.disj_concat l r
    in
    let determinise_list ctx lrs =
      List.fold_left (bin_merge ctx) (List.hd lrs) (List.tl lrs)
    in
    let force_traverse ctx lr =
      hl.force_traverse ctx (disj.disj_splitL lr);
      hr.force_traverse ctx (disj.disj_splitR lr)
    in
    let to_string ctx lr =
      hl.to_string ctx (disj.disj_splitL lr)
      ^ " (+) "
      ^ hr.to_string ctx (disj.disj_splitR lr)
    in
    let tlr = disj.disj_concat hl.head hr.head in
    { head = tlr; determinise_list; force_traverse; to_string }
end

let unit =
  Deterministic
    ( Head.make_key (),
      Lazy.from_val
        {
          head = ();
          determinise_list = (fun _ _ -> ());
          force_traverse = (fun _ _ -> ());
          to_string = (fun _ _ -> "end");
        } )

let determinise t =
  let _, h = Determinise.determinise (Head.make ()) t in
  let h = Lazy.force h in
  h.force_traverse (Head.make ()) h.head;
  h.head

let ensure_determinised = function
  | Deterministic (_, t) -> (Lazy.force t).head
  | _ -> failwith "not determinised"