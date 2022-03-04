open Rows

module rec StateHash : sig
  type t

  type 'a det = {
    body : 'a;
    determinise_list : t -> 'a list -> 'a;
    force_traverse : t -> 'a -> unit;
    to_string : t -> 'a -> string;
  }

  type 'a value = 'a det lazy_t

  include PolyHash.S with type t := t and type 'a value := 'a value
end = struct
  module Hash = PolyHash.Make (StateHash)
  include Hash

  type context = Hash.t

  type 'a det = {
    body : 'a;
    determinise_list : context -> 'a list -> 'a;
    force_traverse : context -> 'a -> unit;
    to_string : context -> 'a -> string;
  }

  type 'a value = 'a det lazy_t
end

type context = StateHash.t

type 'a det = 'a StateHash.det = {
  body : 'a;
  determinise_list : context -> 'a list -> 'a;
  force_traverse : context -> 'a -> unit;
  to_string : context -> 'a -> string;
}

let determinise_list_lazy ctx id hds =
  match StateHash.lookup ctx id with
  | Some v -> v
  | None ->
      let hd =
        lazy
          (let hd = Lazy.force (List.hd hds) in
           let body =
             hd.determinise_list ctx
               (List.map (fun hd -> (Lazy.force hd).body) hds)
           in
           {
             body;
             determinise_list = hd.determinise_list;
             force_traverse = hd.force_traverse;
             to_string = hd.to_string;
           })
      in
      StateHash.add_binding ctx id hd;
      hd

let try_cast_and_merge_lazy ctx id constrA constrB headA headB =
  let headA = Lazy.force headA and headB = Lazy.force headB in
  match Rows.cast_if_constrs_are_same constrA constrB headB.body with
  | Some contB_body -> begin
      match StateHash.lookup ctx id with
      | Some v -> Some v
      | None ->
          let body = headA.determinise_list ctx [ headA.body; contB_body ] in
          let det = Lazy.from_val { headA with body } in
          StateHash.add_binding ctx id det;
          Some det
    end
  | None -> None


type 't state_id = 't StateHash.key

type _ t =
  | Deterministic : 'obj state_id * 'obj det lazy_t -> 'obj t
      (** A state with deterministic transitions. Note that the following states
          are not necessarily deterministic. *)
  | Epsilon : 'a t list -> 'a t  (** Epsilon transitions (i.e. merging) *)
  | InternalChoice : 'lr state_id * ('lr, 'l, 'r) disj * 'l t * 'r t -> 'lr t
      (** Internal choice, splitted by a disjoint concatenation (disj) *)
  | Loop : 'a t lazy_t -> 'a t  (** Start of the fix-loop *)

exception UnguardedLoop

let deterministic id obj = Deterministic (id, obj)
let internal_choice disj l r = InternalChoice (StateHash.make_key (), disj, l, r)
let merge l r = Epsilon [ l; r ]
let loop t = Loop t

let force_traverse ctx t =
  match t with
  | Deterministic (sid, h) when StateHash.lookup ctx sid = None ->
      StateHash.add_binding ctx sid h;
      let h = Lazy.force h in
      h.force_traverse ctx h.body
  | Deterministic (_, _) -> ()
  | _ -> failwith "Impossible: force: channel not determinised"

let rec to_string : 'a. context -> 'a t -> string =
 fun ctx -> function
  | Deterministic (sid, hd) ->
      if Lazy.is_val hd then (
        StateHash.add_binding ctx sid hd;
        let hd = Lazy.force hd in
        hd.to_string ctx hd.body)
      else "<lazy_det>"
  | Epsilon ts -> List.map (to_string ctx) ts |> String.concat " [#] "
  | InternalChoice (_, _, l, r) ->
      let lstr = to_string ctx l and rstr = to_string ctx r in
      lstr ^ " (+#) " ^ rstr
  | Loop t ->
      if Lazy.is_val t then to_string ctx (Lazy.force t) else "<lazy_loop>"

module Determinise : sig
  val determinise : context -> 's t -> 's state_id * 's det lazy_t
end = struct
  type 'a merged_or_backward_epsilon =
    ('a state_id * 'a det lazy_t list, 'a t list) Either.t

  let rec mem_phys k = function
    | x :: xs -> k == x || mem_phys k xs
    | [] -> false

  let fail_unguarded () = raise UnguardedLoop

  let rec determinise : 's. context -> 's t -> 's state_id * 's det lazy_t
      =
   fun ctx st ->
    match epsilon_closure ~ctx ~visited:[] st with
    | Left (sid, hds) -> (sid, determinise_list_lazy ctx sid hds)
    | Right _ -> fail_unguarded ()

  and epsilon_closure :
      type a.
      ctx:context ->
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
                List.fold_left StateHash.make_union_keys (List.hd ids) (List.tl ids)
              in
              ret_merged (id, List.concat hds)
            else
              (* all transitions are epsilon - verify guardedness ==== *)
              let backward = List.concat backward in
              detect_cycles ~visited backward

  and determinise_internal_choice :
        'lr 'l 'r.
        context -> ('lr, 'l, 'r) disj -> 'l t -> 'r t -> 'lr det =
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
    let tlr = disj.disj_concat hl.body hr.body in
    { body = tlr; determinise_list; force_traverse; to_string }
end

let unit =
  Deterministic
    ( StateHash.make_key (),
      Lazy.from_val
        {
          body = ();
          determinise_list = (fun _ _ -> ());
          force_traverse = (fun _ _ -> ());
          to_string = (fun _ _ -> "end");
        } )

let determinise t =
  let _, h = Determinise.determinise (StateHash.make ()) t in
  let h = Lazy.force h in
  h.force_traverse (StateHash.make ()) h.body;
  h.body

let ensure_determinised = function
  | Deterministic (_, t) -> (Lazy.force t).body
  | _ -> failwith "not determinised"